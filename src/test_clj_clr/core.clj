(ns test-clj-clr.core
  (:use clojure.repl clojure.pprint)
  (:require [test-clj-clr.asynchronous-server-socket :as sock]))

(defn -main
  [& args]
  (apply println "Received args:" args)
  (binding [*print-length* 20,
            *print-level* 20]
    (sock/start-listening)))
