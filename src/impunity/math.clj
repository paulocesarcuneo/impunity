(ns impunity.math
  (:refer-clojure :exclude [and or rem]))

(defprotocol IMath
  (div [vm a b])
  (mul [vm a b])
  (neg [vm a b])
  (rem [vm a b])
  (shl [vm a b])
  (shr [vm a b])
  (sub [vm a b])
  (to-b [vm a])
  (to-c [vm a])
  (to-d [vm a])
  (to-f [vm a])
  (to-i [vm a])
  (to-l [vm a])
  (to-s [vm a])
  (ushr [vm a b]))

(defprotocol ILogic
  (add [vm a b])
  (and [vm a b])
  (cmp [vm a b])
  (cmpeq? [vm a b])
  (cmpg [vm a b])
  (cmpge? [vm a b])
  (cmpgt? [vm a b])
  (cmpl [vm a b])
  (cmple? [vm a b])
  (cmplt? [vm a b])
  (cmpne? [vm a b])
  (eq? [vm a])
  (ge? [vm a])
  (gt? [vm a])
  (le? [vm a])
  (lt? [vm a])
  (ne? [vm a])
  (nonnull? [vm a])
  (null? [vm a])
  (or [vm a b])
  (xor [vm a b])
  (instanceof [vm desc a]))

(extend-protocol IMath
  Object
  (div [vm a b] {:op :div :args [a b]})
  (mul [vm a b] {:op :mul :args [a b]})
  (neg [vm a b] {:op :neg :args [a b]})
  (rem [vm a b] {:op :rem :args [a b]})
  (shl [vm a b] {:op :shl :args [a b]})
  (shr [vm a b] {:op :shr :args [a b]})
  (sub [vm a b] {:op :sub :args [a b]})
  (to-b [vm a] {:op :to-b :args [a]})
  (to-c [vm a] {:op :to-c :args [a]})
  (to-d [vm a] {:op :to-d :args [a]})
  (to-f [vm a] {:op :to-f :args [a]})
  (to-i [vm a] {:op :to-i :args [a]})
  (to-l [vm a] {:op :to-l :args [a]})
  (to-s [vm a] {:op :to-s :args [a]})
  (ushr [vm a b] {:op :ushr :args [a b]}))

(extend-protocol ILogic
  Object
  (add [vm a b] false)
  (and [vm a b] false)
  (cmp [vm a b] false)
  (cmpeq? [vm a b] false)
  (cmpg [vm a b] false)
  (cmpge? [vm a b] false)
  (cmpgt? [vm a b] false)
  (cmpl [vm a b] false)
  (cmple? [vm a b] false)
  (cmplt? [vm a b] false)
  (cmpne? [vm a b] false)
  (eq? [vm a] false)
  (ge? [vm a] false)
  (gt? [vm a] false)
  (le? [vm a] false)
  (lt? [vm a] false)
  (ne? [vm a] false)
  (nonnull? [vm a] false)
  (null? [vm a] false)
  (or [vm a b] false)
  (xor [vm a b] false)
  (instanceof [vm desc a] false))