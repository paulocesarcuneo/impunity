(ns impunity.vm
  (:require [clojure.algo.monads :refer :all]
            [impunity.utils :refer :all]
            [impunity.monad :refer :all])
  (:import [impunity.monad Choice]))

;; VM interface
(defprotocol IRead
  (get-mem [vm ref i])
  (peek-stack
    [vm]
    [vm i]))

(defprotocol IOperation
  (operation
    [vm op a b]
    [vm op a]))

(defprotocol ICondition
  (condition  [vm cond]))

(defprotocol IWrite
  (new-obj         [vm obj])
  (set-mem         [vm ref i v])
  (clear-stack     [vm])
  (push-stack      [vm v])
  (push-stack-many [vm vs])
  (pop-stack
    [vm]
    [vm i]))

(defprotocol IJump
  (invoke [vm method args])
  (athrow [vm ex]))

(declare run)

(def class-db (atom {}))

(defn get-method [owner name desc]
  (let [class  (get-in @class-db owner)
        method (get class name)]
    method))

;; VM Instructions
(defn throw-exception [vm]
  (not-implemented! "throw-exception"))

(defn proc2with [vm f2]
  (let [[a b]  (peek-stack vm 2)]
    (m-> vm
         (pop-stack 2)
         (operation f2 a b))))

(defn proc1with [vm f1]
  (let [a      (peek-stack vm)]
    (m-> vm
          (pop-stack)
          (operation f1 a))))

(defmulti  heap-op  (fn [vm insn] (first insn)))

(defmethod heap-op  :arraylength
  [vm _]
  (let [array (get-mem vm (peek-stack vm) :vm/length)]
    (m-> vm
          pop-stack
          (push-stack (:length array)))))

(defmethod heap-op  :newarray
  [vm [_ operand]]
  (new-obj vm {::type   :array
               ::length operand}))

(defmethod heap-op :new
  [vm [_ desc]]
  (new-obj vm {::type desc}))

(defmethod heap-op :anewarray
  [vm [_ desc]]
  (m-> vm
        (pop-stack)
        (new-obj {::type   desc
                  ::length (peek-stack vm)})))

(defmethod heap-op :getstatic
  [vm [_ owner name desc]]
  (push-stack vm
        (get-mem vm owner name)))

(defmethod heap-op :putstatic
  [vm [_ owner name desc]]
  (m-> vm
        (pop-stack)
        (set-mem owner name (peek-stack vm))))

(defmethod heap-op :getfield
  [vm [_ owner name desc]]
  (push-stack vm
        (get-mem vm (peek-stack vm) name)))

(defmethod heap-op :putfield
  [vm [_ owner name desc]]
  (let [[v ref] (peek-stack vm 2)]
    (m-> vm
          (pop-stack 2)
          (set-mem ref name v))))

(defmethod heap-op :load
  [vm [_ var desc]]
   (push-stack vm (get-mem vm :local var)))

(defmethod heap-op :store
  [vm [_ var desc]]
  (m-> vm
        (pop-stack)
        (set-mem :local var (peek-stack vm))))

(defmethod heap-op :aload
  [vm [_ desc]]
  (let [[i array] (peek-stack vm 2)]
    (m-> vm
          (pop-stack  2)
          (push-stack (get-mem vm array i)))))

(defmethod heap-op :astore
  [vm [_ desc]]
  (let [[val i ref] (peek-stack vm 3)]
    (m-> vm
          (pop-stack 3)
          (set-mem ref i val))))

(defmulti  control-op (fn [vm [nmonic] method pc]
                        nmonic))

(defmethod control-op :athrow
  [vm _ _ _]
  (not-implemented! "athrow"))

(defmethod control-op :monitor
  [vm _ _ _]
  (not-implemented! "monitor"))

(defmethod control-op :ret
  [vm [_ var desc] _ _]
  (not-implemented! "RET"))

(defmethod control-op :return
  [vm [_ desc] _ _]
  (if (= desc :V)
    (clear-stack vm)
    (m-> vm
         (clear-stack)
         (push-stack (peek-stack vm)))))

(defmethod control-op :invoke
  [vm [_ kind owner name [ret args] :as insn] _ _]
  (let [method     (get-method  owner name [ret args])
        args-count (case kind
                     :static (count args)
                     (inc (count args)))]
    (m-> vm
          (pop-stack args-count) 
          (invoke insn
                  (peek-stack vm args-count)))))


(defmethod control-op :checkcast
  [vm [_ desc] method pc]
  (let [ref    (peek-stack vm)]
    (plus (condition vm   [true :instanceof desc ref])
          (delay (m-> vm
                      (condition [false :instanceof desc ref])
                      (athrow {::type ClassCastException}))))))

(defmethod control-op :if
  [vm [_ label cmp maybe-dec] method pc]
  (case cmp
    (:eq 
     :ne 
     :lt 
     :ge 
     :gt 
     :le
     :null
     :nonnull)
    (let [a      (peek-stack vm)
          result (operation vm cmp a)
          vm     (pop-stack vm)]
      (plus (m-> vm
                 (condition [false result cmp a])
                 (run method (inc pc)))
            (delay (m-> vm
                        (condition [true result cmp a])
                        (run method label)))))
    
    (:cmpeq 
     :cmpne 
     :cmplt 
     :cmpge 
     :cmpgt 
     :cmple)
    (let [[a b]  (peek-stack vm 2)
          result (operation vm cmp a b)
          vm     (pop-stack vm 2)]
      (plus (m-> vm
                 (condition [false result cmp a b])
                 (run method (inc pc)))
            (delay (m-> vm
                        (condition [true result cmp a b])
                        (run method label)))))

    :goto    (if (> label pc)
               (delay (run vm method label))
               (assoc vm :loop true))
    :jsr     (not-implemented! "JSR")))

(defmulti  stack-op   (fn [vm insn] (first insn)))

(defmethod stack-op :default
  [vm [name]]
  (not-implemented! name))

(doseq [insn-name [:label
                   :frame
                   :linenumber
                   :nop
                   :2]]
  (defmethod stack-op insn-name
    [vm _]
    vm))

(defmethod stack-op :bipush
  [vm [_ operand]]
  (push-stack vm operand))

(defmethod stack-op :sipush
  [vm [_ operand]]
  (push-stack vm operand))

(defmethod stack-op :instanceof
  [vm [_ desc]]
  (let [a  (peek-stack vm)]
    (m-> vm
          (pop-stack)
          (operation :instanceof desc a))))

(defmethod stack-op :const
  [vm [_ val desc]]
  (push-stack vm val))

(defmethod stack-op :pop
  [vm [_ times]]
  (pop-stack vm times))

(defmethod stack-op :dup
  [vm [_ depth]]
  (let [dup   (peek-stack vm)
        head  (peek-stack vm depth)]
    (m-> vm
          (pop-stack depth)
          (push-stack dup)
          (push-stack-many head))))

(defmethod stack-op :dup2
  [vm [_ depth]]
  (let [dups (peek-stack vm 2)
        head (peek-stack vm depth)]
    (m-> vm
          (pop-stack depth)
          (push-stack-many dups)
          (push-stack-many head))))

(defmethod stack-op :swap
  [vm _]
  (let [[v1 v2] (peek-stack vm 2)]
    (m-> vm
          (pop-stack 2)
          (push-stack-many [v2 v1]))))

(doseq [op [:add
            :and
            :cmp      
            :cmpg   
            :cmpg    
            :cmpl    
            :div
            :mul           
            :or
            :rem
            :shl
            :shr
            :sub
            :ushr
            :xor
            :add]]
  (defmethod stack-op op
    [vm [op]]
    (proc2with vm op)))

(defmethod stack-op :new
  [vm _]
  (proc1with vm :neg))

(defrecord VirtualMachine [heap
                           local                           
                           stack
                           events
                           border?]
  IRead
  (get-mem [vm ref i]
    (if (= ref :local)
      (get local i)
      (get-in heap [ref i])))

  (peek-stack [vm]
    (peek stack))
  
  (peek-stack [vm i]
    (take i stack))

  IWrite
  (new-obj [vm obj]
    (let [i (gen-id)]
      (push-stack (assoc-in vm [:heap i] obj)
                  i)))
  
  (set-mem  [vm ref i v]
    (if (= ref :local)
      (assoc-in vm [:local i] v)
      (assoc-in vm [:heap i] v)))
  
  (push-stack [vm v]
    (assoc vm :stack (conj stack v)))
  
  (push-stack-many [vm vs]
    (assoc vm :stack (apply list (concat vs stack))))

  (pop-stack [vm]
    (assoc vm :stack (pop stack)))

  (pop-stack [vm i]
    (assoc vm :stack (apply list (drop i stack))))

  (clear-stack [vm]
    (assoc vm :stack '()))

  ICondition
  (condition [vm cond]
    (assoc vm :events (conj events cond)))

  IOperation
  (operation [vm op a]
    (lvar))
  
  (operation [vm op a b]
    (lvar))

  IJump
  (invoke [caller method args]
    (if (border? method)
      (let [result (lvar)]
        (-> caller
            (condition  [:invoke result method args])
            (push-stack result)))
      (let [callee  (run (VirtualMachine. heap
                                          (vec args)
                                          '()
                                          events
                                          border?
                                          method
                                          0)
                      method 0)]
        (let [{:keys [heap stack events]} callee]
          (assoc caller
                 :heap   heap
                 :events events
                 :stack  (concat stack (:stack caller)))))))

  (athrow [vm ex]
    (assoc vm :exception ex))
 
  IBind
  (bind [m f]
    (f m))
  (stream [m]
    [m]))

(defn vm []
  (VirtualMachine. {} [] '() [] (constantly true)))

(defn step [vm method pc]
  (let [insn (nth (:method/instructions method) pc)]
    (case (:kind (meta insn))              
      :control      (control-op vm insn method pc)
      :stack        (stack-op vm insn)
      :heap         (heap-op  vm insn) 
      :nop          vm)))

(defn run [vm method pc]
  (let [insns (:method/instructions method)
        size  (count insns)]
    (loop [vm vm
           pc pc]
      (if (>= pc size) 
        vm
        (let [vm' (step vm method pc)]
          (if (instance? Choice vm')
            vm'
            (recur vm'
                   (inc pc))))))))

(defn run-method [{[_ args] :type :as method}]
  (let [vm (assoc (vm)
                  :local [(lvar) (lvar)])]
    (run vm method 0)))





