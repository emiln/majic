(ns majic.core-test
  (:require [clojure.test :refer :all]
  	        [midje.sweet :refer :all]
            [majic.core :refer :all]))

(deftest main-test
  (facts "About the -main function"
    (fact "It should print a sensible error message when called without arguments."
      (with-out-str (-main)) => #".+")
    (fact "It should print a card when called with a card ID."
      (with-out-str (-main "1234")) => #".+")))