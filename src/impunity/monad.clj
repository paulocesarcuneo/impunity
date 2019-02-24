(ns impunity.monad)

(defprotocol IBind
  (bind   [m   f])
  (stream [m]))

(extend-protocol IBind
  clojure.lang.Delay
  (bind [d f]
    (delay (f @d)))
  
  (stream [d]
    [@d]))


(defrecord Choice [h t]
  IBind
  (bind [_ f]
    (Choice. (f h)
             (f t)))
  
  (stream [v]
    (concat (stream h)
            (lazy-seq (stream t)))))

(defn plus   [m1 m2]
  (Choice. m1 m2))


(def id-gen (atom 0))
(defn gen-id []
  (swap! id-gen inc))

(defrecord Var [i])
(defn lvar []
  (Var. (gen-id)))

(defmethod print-method Var [^Var v ^java.io.Writer w]
  (.write w (str "_" (.-i v))))

(defmacro m-> [vm & fs]
  (letfn [(to-fn [exp]
            (cond
              (symbol? exp) exp
              (and (list? exp)
                   (= 1 (count exp))) (first exp)
              (list? exp)  `(fn [x#] (~(first exp) x# ~@(rest exp)))))]
    (reduce (fn [a b]
              `(bind ~a ~(to-fn b)))
            vm
            fs)))

(defmacro mlet [binds & body]
  (let [binds        (partition 2 binds)]
    (reduce (fn [body [exp val]]
              `(bind ~val
                     (fn [~exp]
                       ~body)))
            `(do ~@body)
            binds)))
