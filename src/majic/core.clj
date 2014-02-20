(ns majic.core
  (:require [majic.parser :refer [card-by-id]]
            [clojure.pprint :refer [pprint]]))

(defn -main
  [& args]
  (let [id (Integer/parseInt (first args))]
  	(pprint (card-by-id id))))
