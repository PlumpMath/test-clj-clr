(ns test-clj-clr.encoding
  (:require clojure.string)
  (:import [System Convert]))

(defn qwik-encode [s]
  (str
    (clojure.string/join " "
      (map int s))
    "<EOF>"))

(defn qwik-decode [s]
  (try
    (apply str
      (map #(char (Convert/ToInt32 %))
        (clojure.string/split
          ((re-matches #"(.*?)<EOF>" s) 1)
          #" ")))
    (catch Exception e
      (str "had trouble. this doesn't parse good: " s))))
