(ns test-clj-clr.encoding
  (:import [System Convert]))

(defn qwik-encode [s]
  (str
    (clojure.string/join " "
      (map int s))
    "<EOF>"))

(defn qwik-decode [s]
  (apply str
    (map #(char (Convert/ToInt32 %))
      (clojure.string/split
        ((re-matches #"(.*?)<EOF>" s) 1)
        #" "))))
