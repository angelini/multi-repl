(defproject multi-repl "0.1.0-SNAPSHOT"
  :description "Multi language REPL controller"
  :url "https://github.com/angelini/multi-repl"
  :license {:name "The MIT License (MIT)"
            :url "https://github.com/angelini/multi-repl/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [me.raynes/conch "0.8.0"]
                 [com.stuartsierra/component "0.2.3"]
                 [clojure-msgpack "1.0.0"]]
  :main ^:skip-aot multi-repl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
