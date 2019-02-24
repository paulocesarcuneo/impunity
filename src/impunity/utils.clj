(ns impunity.utils)

(defmacro not-implemented! [msg]
  `(throw (ex-info (str "Not Implemented: " ~msg) {})))


(defmacro let-obj [obj fields & body]
  `(let [~@(mapcat (fn [n]
                     [(symbol (second (re-matches #"(?:.-|.)(.*)"
                                                  (name n))))
                      `(~n ~obj)
                      ])
                   fields)]
     ~@body))

(defmacro case+
  "Same as case, but evaluates dispatch values, needed for referring to
   class and def'ed constants as well as java.util.Enum instances."
  [value & clauses]
  (let [clauses (partition 2 2 nil clauses)
        default (when (-> clauses last count (== 1))
                  (last clauses))
        clauses (if default (drop-last clauses) clauses)
        eval-dispatch (fn [d]
                        (if (list? d)
                          (map eval d)
                          (eval d)))]
    `(case ~value
       ~@(concat (->> clauses
                   (map #(-> % first eval-dispatch (list (second %))))
                   (mapcat identity))
           default))))

(defn tacnoc [tail head]
  (apply list (concat head tail)))
