(ns test-clj-clr.core
  (:use clojure.repl clojure.pprint))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  [& args]
  (apply println "Received args:" args))
