(ns majic.util
  (:require [clojure.string :as s :refer [lower-case trim]]))

(defn string->keyword
  [s]
  (or (some-> s
        lower-case
        trim
        (s/replace #"\s+" "-")
        keyword)
      :unknown))

(defn string->int
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException n nil)))

(defn file->int-seq
  [file-path]
  (with-open [rdr (clojure.java.io/reader file-path)]
    (some->> rdr
      line-seq
      (map #(re-seq #"\d+" %))
      flatten
      (map string->int))))