(ns majic.util-test
  (:require [clojure.test :refer :all]
  	        [midje.sweet :refer :all]
            [majic.util :refer :all]))

(deftest util-test
  (facts "About string->keyword"
    (fact "It should work for trivial examples."
      (string->keyword "White") => :white
      (string->keyword "Tap") => :tap)
    (fact "It should work for more complex examples."
      (string->keyword "Black or Green") => :black-or-green
      (string->keyword "A tricky ol' string") => :a-tricky-ol'-string))
  (facts "About string->int"
  	(fact "It should parse a bunch of numbers."
  	  (string->int "0") => 0
  	  (string->int "123") => 123
  	  (string->int "9013593195") => 9013593195)
  	(fact "It should return nil if supplied garbage."
  	  (string->int "herpaderpa") => nil
  	  (string->int "") => nil))
  (facts "About n-partitions"
  	(fact "It should partition a small range properly."
  	  (n-partitions 5 (range 10)) => '((0 5) (1 6) (2 7) (3 8) (4 9)))
  	(fact "It should partition unevenly if necessary."
  	  (n-partitions 3 (range 10)) => '((0 3 6 9) (1 4 7 nil) (2 5 8 nil)))))