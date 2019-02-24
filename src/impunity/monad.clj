(ns impunity.monad)

;; Non-Determinism System
(defprotocol IBind
  (bind   [m   f])
  (stream [m]))

(extend-protocol IBind
  Object
  (bind [m f]
    (f m))
  (stream [m]
    [m])

  nil
  (bind [_ _]
    nil)
  (stream [_]
    [])
  
  clojure.lang.Delay
  (bind [d f]
    (delay (bind @d f)))
  (stream [d]
    (stream @d)))


(deftype Choice [h t]
  IBind
  (bind [_ f]
    (Choice. (bind h f)
             (bind t f)))
  
  (stream [v]
    (concat (stream h)
            (lazy-seq (stream t)))))

(defn choice [m1 m2]
  (Choice. m1 m2))

(defmacro plus [m1 m2]
  `(choice ~m1 (delay ~m2)))

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

(defn m-all [m fs]
  (reduce bind m fs))

(defmacro all [& fs]
  `(fn [s#]
     (m-all s# [~@fs])))

(defn m-reduce [f m vs]
  (reduce (fn [m v]
            (bind m #(f % v)))
          m
          vs))

;; Constraint System

(def id-gen (atom 0))
(defn gen-id []
  (swap! id-gen inc))

(defprotocol IWalk
  (walk [v smap]))

(extend-protocol IWalk
  nil
  (walk [_ _]
    nil)
  
  Object
  (walk [v _]
    v))

(defprotocol IUnifyTerm
  (unify-term [u v smap]))

(extend-protocol IUnifyTerm
  Object
  (unify-term [u v smap]
    (when (= u v)
      smap))
  
  nil
  (unify-term [u v smap]
    (when (nil? v)
      smap))
  
  clojure.lang.Sequential
  (unify-term [u v smap]
    (when (and (sequential? v)
               (= (count u)
                  (count v)))
      (m-reduce (fn [smap [u v]]
                  (unify-term u v smap))
                smap
                (map vector u v)))))


(deftype Var [name type]
  Object
  (hashCode [_]
    (hash-combine (hash name) type))
  (equals [_ other]
    (boolean
     (when (instance? Var other)
       (and (= name (.-name ^Var other))
            (= type (.-type ^Var other))))))
  
  IWalk
  (walk [v smap]
    (loop [v v]
      (let [v' (get smap v ::notfound)] 
        (cond
          (= v'  ::notfound) v
          (instance? Var v') (recur v')
          :else              v'))))
  
  IUnifyTerm
  (unify-term [v u {constraints :constraints
                    :as smap}]
    (if (lvar? u)
      (m-> smap
           (assoc u v)
           (assoc :constraints (-> constraints
                                   (dissoc u)
                                   (update v conj (get constraints u)))))
      (m-> smap
           (assoc v u)
           (m-all (get-in smap [:constraints v]))))))

(defn lvar
  ([]
   (Var. (gen-id) nil))
  ([type]
   (Var. (gen-id) type)))

(defn lvar? [v]
  (instance? Var v))

(defmethod print-method Var [v ^java.io.Writer w]
  (let [type (when-let [type (.-type ^Var v)]
               (str "^" type " "))
        name (str "_" (.-name ^Var v))]
    (.write w (clojure.string/replace (str type  name)
                                      ";"
                                      ""))))

(defn unify
  ([v u]
   (fn [smap]
     (unify v u smap)))
  ([v u smap]
   (let [v (walk v smap)
         u (walk u smap)]
     (cond
       (identical? v u) smap
       (lvar? v) (unify-term v u smap)
       :else     (unify-term u v smap)))))

;; Constraints
(defn add-constraint [smap a g]
  (update-in smap
             [:constraints a] (fnil conj #{}) g))


(defn rm-constraint [smap a g]
  (update-in smap
             [:constraints a] disj g))

(defprotocol IInfo
  (info [i]))

(defn p->constraint
  ([p? a]
   (reify
     clojure.lang.IFn
     (invoke [this smap]
       (let [a' (walk a smap)]
         (if (or (lvar? a'))
           smap
           (when (p? a')
             (rm-constraint smap a this)))))
     IInfo
     (info [this]
       {:p? p?
        :args [a]})))
  
  ([p? a b]
   (reify
     clojure.lang.IFn
     (invoke [this smap]
       (let [a' (walk a smap)
             b' (walk b smap)]
         (if (or (lvar? a')
                 (lvar? b'))
           smap
           (when (p? a' b')
             (-> smap
                 (rm-constraint a this)
                 (rm-constraint b this))))))
     IInfo
     (info [this]
       {:p? p?
        :args [a b]}))))

(defn c-lift
  ([p? a]
   (fn [smap]
     (let [c (p->constraint p? a)]
       (add-constraint smap a c))))
  ([p? a b]
   (fn [smap]
     (let [c (p->constraint p? a b)]
       (-> smap
           (add-constraint a c)
           (add-constraint b c))))))





