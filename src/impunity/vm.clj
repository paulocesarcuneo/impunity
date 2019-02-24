(ns impunity.vm
  (:require [impunity.asm :refer [load-method class-by-name]]
            [impunity.utils :refer :all]
            [impunity.monad :refer :all])
  (:import  [impunity.monad Choice]))

;; VM interface
(defprotocol IRead
  (get-mem [vm ref i])
  (peek-stack
    [vm]
    [vm i]))


(defprotocol IWrite
  (new-obj         [vm obj])
  (set-mem         [vm ref i v])
  (clear-stack     [vm])
  (push-stack      [vm v])
  (push-stack-many [vm vs])
  (pop-stack
    [vm]
    [vm i]))

(defprotocol IOperation
  (operation
    [vm op a b]
    [vm op a]))

(defprotocol ICondition
  (record [vm condition]))

(defprotocol IControl
  (step   [vm])
  (return [vm])
  (check
    [vm ex cond a]
    [vm ex cond a b])

  (jump
    [vm label]
    [vm label cond a]
    [vm label cond a b])
  
  (invoke [vm method args])
  (athrow [vm ex]))

(declare run)

;; VM Instructions
(defn exception [class-name]
  {::type class-name})

(defmulti  play  (fn [vm insn]
                   (first insn)))

(defmethod play :default
  [vm [nmonic]]
  (not-implemented! nmonic))

(doseq [insn-name [:label
                   :frame
                   :linenumber
                   :nop]]
  (defmethod play insn-name
    [vm _]
    vm))

;; Heap Ops
(defmethod play  :arraylength
  [vm _]
  (let [array (get-mem vm (peek-stack vm) :vm/length)]
    (m-> vm
         (pop-stack)
         (check (exception "java/lang/NullPointerException")
                :nonnull array)
         (push-stack (:length array)))))

(defmethod play  :newarray
  [vm [_ operand]]
  (m-> vm
       (check (exception "java/lang/NegativeArraySizeException")
              :cmpge operand 0)
       (new-obj {::type   :array
                 ::length operand})))

(defmethod play :new
  [vm [_ desc]]
  (new-obj vm {::type desc}))

(defmethod play :anewarray
  [vm [_ desc]]
  (let [operand (peek-stack vm)]
    (m-> vm
         (pop-stack)
         (check (exception "java/lang/NegativeArraySizeException")
                :cmpge operand 0)
         (new-obj {::type   desc
                   ::length operand}))))

(defmethod play :getstatic
  [vm [_ class-name field-name desc]]
  (push-stack vm
        (get-mem vm class-name field-name)))

(defmethod play :putstatic
  [vm [_ class-name field-name desc]]
  (m-> vm
        (pop-stack)
        (set-mem class-name field-name (peek-stack vm))))

(defmethod play :getfield
  [vm [_ class-name field-name desc]]
  (let [obj (peek-stack vm)]
    (m-> vm
         (pop-stack)
         (push-stack (get-mem vm
                              obj
                              field-name)))))

(defmethod play :putfield
  [vm [_ class-name field-name desc]]
  (let [[v ref] (peek-stack vm 2)]
    (m-> vm
          (pop-stack 2)
          (set-mem ref field-name v))))

(defmethod play :load
  [vm [_ var desc]]
   (push-stack vm (get-mem vm :local var)))

(defmethod play :store
  [vm [_ var desc]]
  (m-> vm
        (pop-stack)
        (set-mem :local var (peek-stack vm))))

(defmethod play :aload
  [vm [_ desc]]
  (let [[i array] (peek-stack vm 2)]
    (m-> vm
         (pop-stack  2)
         (check (exception "java/lang/NullPointerException")
                :nonnull array)
         (check (exception "java/lang/ArrayIndexOutOfBoundsException")
                :cmpg (::lenght array) i)
         (push-stack (get-mem vm array i)))))

(defmethod play :astore
  [vm [_ desc]]
  (let [[val i array] (peek-stack vm 3)]
    (m-> vm
         (pop-stack 3)
         (check (exception "java/lang/NullPointerException")
                :nonnull array)
         (check (exception "java/lang/ArrayIndexOutOfBoundsException")
                :cmpg (::length array) i)
         (set-mem array i val))))

;; Control Ops
;; TODO implement :monitor
(defmethod play :athrow
  [vm _]
  ;; TODO check monitor
  (let [ex (peek-stack vm)]
    (m-> vm
         (check (exception "java/lang/NullPointerException")
                :nonnull ex)
         (athrow ex))))

(defmethod play :return
  [vm [_ desc]]
  ;; TODO implement monitor check
  (if (= desc :V)
    (m-> vm
         (clear-stack)
         (return))
    (m-> vm
         (clear-stack)
         (push-stack (peek-stack vm))
         (return))))

(defmethod play :invoke
  [vm [_ kind class-name method-name [ret args]]]
  (let [method     (load-method  class-name method-name args)
        args-count (count args)]
    (if (= kind :static)
      (m-> vm
           (pop-stack args-count) 
           (invoke method (peek-stack vm args-count)))
      (let [args-count      (inc args-count)
            [this :as args] (peek-stack vm args-count)]
        (m-> vm
             (pop-stack args-count)
             (check (exception "java/lang/NullPointerException")
                    :nonnull this)
             (invoke method args))))))


(defmethod play :checkcast
  [vm [_ desc]]
  (let [ref    (peek-stack vm)]
    (m-> vm
         (check (exception "java/lang/ClassCastException")
                :instanceof desc ref))))

(defmethod play :if
  [vm [_ label cmp maybe-dec]]
  (case cmp
    (:eq 
     :ne 
     :lt 
     :ge 
     :gt 
     :le
     :null
     :nonnull)
    (let [a (peek-stack vm)]
      (jump vm label cmp a))
    
    (:cmpeq 
     :cmpne 
     :cmplt 
     :cmpge 
     :cmpgt 
     :cmple)
    (let [[a b]  (peek-stack vm 2)]
      (jump vm label cmp a b))

    :goto    (jump vm label)
    :jsr     (not-implemented! "JSR")))


;; StackOps
(defmethod play :bipush
  [vm [_ operand]]
  (push-stack vm operand))

(defmethod play :sipush
  [vm [_ operand]]
  (push-stack vm operand))

(defmethod play :instanceof
  [vm [_ desc]]
  (let [a  (peek-stack vm)]
    (m-> vm
          (pop-stack)
          (operation :instanceof desc a))))

(defmethod play :const
  [vm [_ val desc]]
  (push-stack vm val))

(defmethod play :pop
  [vm [_ times]]
  (pop-stack vm times))

(defmethod play :dup
  [vm [_ depth]]
  (let [dup   (peek-stack vm)
        head  (peek-stack vm depth)]
    (m-> vm
          (pop-stack depth)
          (push-stack dup)
          (push-stack-many head))))

(defmethod play :dup2
  [vm [_ depth]]
  (let [dups (peek-stack vm 2)
        head (peek-stack vm depth)]
    (m-> vm
          (pop-stack depth)
          (push-stack-many dups)
          (push-stack-many head))))

(defmethod play :swap
  [vm _]
  (let [[v1 v2] (peek-stack vm 2)]
    (m-> vm
          (pop-stack 2)
          (push-stack-many [v2 v1]))))


(defn proc2with [vm f2]
  (let [[a b]  (peek-stack vm 2)]
    (m-> vm
         (pop-stack 2)
         (operation f2 a b))))

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
  (defmethod play op
    [vm [op]]
    (proc2with vm op)))

(defmethod play :neg
  [vm _]
  (let [a (peek-stack vm)]
    (m-> vm
         (pop-stack)
         (operation :neg a))))

(defrecord VirtualMachine [heap
                           method
                           stack
                           exception
                           conditions
                           border?]
  IRead
  (get-mem [vm ref i]
    (if (= ref :local)
      (get-in vm [:method :local i])
      (get-in vm [:heap   ref i])))

  (peek-stack [vm]
    (peek (:stack method)))
  
  (peek-stack [vm i]
    (take i (:stack method)))

  IWrite
  (new-obj [vm obj]
    (let [i (lvar (::type obj))]
      (push-stack (assoc-in vm [:heap i] obj)
                  i)))
  
  (set-mem  [vm ref i v]
    (if (= ref :local)
      (assoc-in vm
                [:method :local i] v)
      (assoc-in vm
                [:heap  i] v)))
  
  (push-stack [vm v]
    (update-in vm
               [:method :stack] conj v))
  
  (push-stack-many [vm vs]
    (update-in vm
               [:method :stack] #(apply list (concat vs %))))

  (pop-stack [vm]
    (update-in vm
               [:method :stack] pop))

  (pop-stack [vm i]
    (update-in vm
               [:method :stack] #(apply list (drop i %))))

  (clear-stack [vm]
    (assoc-in vm
              [:method :stack] '()))

  ICondition
  (condition [vm cond]
    (assoc vm :events (conj events cond)))

  IOperation
  (operation [vm op a]
    (if (lvar? a)
      (lvar)
      (case op
        :neg  (- a)
        :eq   (= a 0)
        :ne   (not= a 0)
        :lt   (< a 0)
        :le   (<= a 0)
        :ge   (>= a 0)
        :gt   (> a 0)
        :null (= nil a)
        :nonnull  (not= nil a)
        :cast   a)))
  
  (operation [vm op a b]
    (if (or (lvar? a)
            (lvar? b))
      (lvar)
      (case op
        :add (+ a b)  
        :sub (- a b)
        :mul (* a b)
        :div (/ a b)
        :rem (rem a b)
        :shl (bit-shift-left a b)
        :shr (bit-shift-right a b)
        :ushr (unsigned-bit-shift-right a b)
        :and (bit-and a b)
        :or (bit-or a b)
        :xor (bit-xor a b)
        :cmp (= a b)
        :cmpl (< a b)
        :cmple (<= a b)
        :cmpg (> a b)
        :cmpge (>= a b))))

  ICondition
  (record [vm condition]
    (update vm :conditions conj condition))
  
  IControl
  (step   [vm]
    (let [{pc    :pc
           insns :method/instructions} method]
      (if-let [insn (and (< pc (count insns))
                         (nth insns pc))]
        (m-> vm
             (play insn)
             (update-in [:method :pc] inc))
        vm)))
  
  (check [vm ex cond a]
    (let [result (operation vm cond a)]
      (case result
        true  vm
        false (athrow vm ex)
        (plus (record vm [true cond a])
              (m-> vm
                   (record [false cond a])
                   (athrow ex))))))
  
  (check [vm ex cond a b]
    (let [result (operation vm cond a b)]
      (case result
        true  vm
        false (athrow vm ex)
        (plus (record vm [true cond a b])
              (m-> vm
                   (record [false cond a b])
                   (athrow ex))))))

  (jump  [vm label]
    (assoc-in vm [:method :pc] label))
  
  (jump  [vm label cond a]
    (let [result (operation vm cond a)]
      (case result
        true   (jump vm label)
        false  vm
        (plus (record vm [false cond a])
              (m-> vm
                   (record [true cond a])
                   (jump label))))))
  
  (jump  [vm label cond a b]
    (let [result (operation vm cond a b)]
      (case result
        true   (jump vm label)
        false  vm
        (plus (record vm [false cond a b])
              (m-> vm
                   (record [true cond a b])
                   (jump label))))))
  
  (invoke [vm callee args]
    (if (border? callee)
      (let [result (lvar)]
        (m-> vm
             (record  [:invoke result (select-keys callee
                                                   [:method/class-name
                                                    :method/name
                                                    :method/type])
                       args])
             (push-stack result)))
      (assoc vm
             :method  (assoc callee
                             :pc    -1 ;;-1 because step
                             :local args)
             :stack   (conj stack method))))
  
  (return [vm]
    (if (empty? stack)
      vm
      (let [top   (peek stack)
            stack (pop stack)]
        (assoc vm
               :method method
               :stack  stack))))

  (athrow [vm {thrown-type ::type :as ex}]
    ;; TODO implement subclass predicate
    (let [{pc        :pc
           trycatchs :method/trycatch}  method
          {:trycatch/keys [handler]}   (some (fn [{:trycatch/keys [start end type]}] 
                                               (and (< start pc end)
                                                    (= type thrown-type)))
                                             trycatchs)]
      (if handler
        (assoc-in vm [:method :pc] handler)
        (m-> (assoc vm :exception ex)
             (clear-stack)
             (return))))))

(defn parse-class-name [name]
  (second (re-matches #"L([^;]+);" name)))

(defn vm [method]
  (let [
        {[ret args] :method/type
         access     :method/access
         class-name :method/class-name } method
        ]
    (map->VirtualMachine {:heap  {}
                          :stack []
                          :method  (assoc method
                                          :pc 0
                                          :local (mapv (fn [class-name]
                                                         (if-let [class-name (parse-class-name class-name)]
                                                           (into {}
                                                                 (map (fn [n]
                                                                        [n (lvar)])
                                                                      (keys (:class/fields
                                                                             (class-by-name
                                                                              class-name)))))
                                                           (lvar)))
                                                       (if (access :static)
                                                         args
                                                         (cons class-name
                                                               args))))
                          :border? (constantly true)
                          :conditions []})))


(defn run [method]
  (loop [vm (vm method)
         i  0]
    (let [vm' (try
                (bind vm step)
                (catch Exception e
                  (throw (ex-info "VM Crash" vm e))))]
      (if (or (> i 100)
              (= vm' vm))
        vm'
        (recur vm'
               (inc i))))))




