(defproject impunity "0.1.0-SNAPSHOT"
  :description "Generartes unit/functional test for class file"
  :url "http://cuneopaulocesar.github.io/impunity"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.ow2.asm/asm-all "5.2"]
                 [org.ow2.asm/asm-all "5.2" :classifier "sources"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.clojure/algo.monads "0.1.6"]]
  :main ^:skip-aot impunity.core
  :target-path "target/%s"
  :java-source-paths ["java"]
  :profiles {:uberjar {:aot :all}})
