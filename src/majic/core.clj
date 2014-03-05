(ns majic.core
  (:require [majic.parser :refer [card-by-id]]
  			[majic.util :refer [string->int]]
            [clojure.pprint :refer [pprint]]))

(defn -main
  [& args]
  (if-let [id (string->int (first args))]
  	(pprint (card-by-id id))
  	(println "You must supply an integer ID to look up in Gatherer.")))
