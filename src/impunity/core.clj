(ns impunity.core
  (:require [impunity.vm :as vm]
            [impunity.asm :as asm]
            [impunity.monad :as m])
  (:gen-class))

#_ "

BACKLOG:
- Is static vs instance distinction needed?

- Optimize stack pop-many:
  clojure.core/drop converts list to lazy seq
  but pop/peek only applies to IPersistentStack
  so currently must call (apply list ...)
  this maybe expensive.

- Improve Memory Model.
  :memory {:local  []
           Class   {:field-name ...}
           #objid  {:field-name ...}}

- Type naming
- Exception Handling
 
TODOs:
- Implement ALL symbolic ALU.
- Implement method dispatch.
- Implement checkcast/instanceof.
- Interpret ALL instructions, almost done.

DOING:
- Implement jump control.
  Will need to track facts, calculated by ALU.

- Implement BASE symbolic ALU.
  May require unification, should interleave minikanren?

DONE:
- Interpret basic instruction set, atleast build a \"value\" tree
- Implement memory model.

"


#_(let [method (asm/load-method "Domain" "complexLogic" ["LIn;"])]
    (nth (iterate #(m/bind % vm/step) (vm/vm method)) 15))

(let [method (asm/load-method "Domain" "complexLogic" ["LIn;"])]
                 (vm/run method))
