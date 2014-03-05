(ns majic.util
  (:require [clojure.string :as s :refer [lower-case trim]]))

(defn string->keyword
  "Returns a keyword of the form:
   :words-in-original-string"
  [s]
  (or (some-> s
        lower-case
        trim
        (s/replace #"\s+" "-")
        keyword)
      :unknown))

(defn string->int
  "Attempts to parse the given string, returning either a number or nil."
  [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException n nil)))

(defn n-partitions
  "Partitions `coll` into `n` sequences, padding with nil as needed.

   Examples:

   (n-partitions 3 (range 10))
   => '((0 3 6 9) (1 4 7 nil) (2 5 8 nil))

   (n-partitions 5 (range 15))
   => '((0 5 10) (1 6 11) (2 7 12) (3 8 13) (4 9 14))"
  [n coll]
  (apply map (fn [& c] (lazy-seq c)) (partition n n (repeat n nil) coll)))
