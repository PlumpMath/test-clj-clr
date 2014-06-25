(ns test-clj-clr.evaluation
  (:use clojure.repl clojure.pprint)
  (:require [clojure.main :as main]))

(defn evaluate-data [d]
  (with-out-str
    (pr
      (eval
        (read-string d)))))
