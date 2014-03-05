(ns majic.documentation-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]))

(def exceptions
  ^{:private true
    :doc "Public mappings that will not be required to provide a docstring.
The format is: [symbol-name namespace-string], where the former is a
quoted symbol and the latter is a string."}
  #{})

(defn- symbol-lacks-doc?
  [sym]
  "Returns true if the given symbol is required to have a docstring, but
doesn't. All symbols defined on a specific line (not auto-generated) that
are not part of the exception set must have a docstring and will be found
lacking if they don't."
  (and ((complement :doc) sym)
       (:line sym)
       ((complement contains?) exceptions
        [(:name sym) (namespace-munge (:ns sym))])))

(defn- docless-mappings-in-ns
  "A set of all public mappings in the given namespace that are required to
have a docstring, but don't actually have one."
  [ns-name]
  (->> ns-name
    ns-publics
    (map second)
    (map meta)
    (filter symbol-lacks-doc?)
    (map :name)
    (into #{})))

(deftest documentation-test
  (facts "About documentation."
    (fact "No public mappings in the parser can be missing docstring."
      (docless-mappings-in-ns 'majic.parser) => #{})
    (fact "No public mappings in the util namespace can be missing docstring."
      (docless-mappings-in-ns 'majic.util) => #{})))