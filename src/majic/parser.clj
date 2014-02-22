(ns majic.parser
  (require [hickory.core :as hick]
           [hickory.select :as sel :refer [child id select tag]]
           [clojure.string :as str]))

(def exp-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_currentSetSymbol")

(def id-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow")

(def url
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s")

(defn- map-rule
  [rule]
  (get {"B" ":tap: Add :black to your mana pool."
        "G" ":tap: Add :green to your mana pool."
        "R" ":tap: Add :red to your mana pool."
        "U" ":tap: Add :blue to your mana pool."
        "W" ":tap: Add :white to your mana pool."}
       rule rule))

(defn- string->edition
  [s]
  (some->> s
    (re-seq #"([\w\s]+)\(([\w\s]+)\)")
    (map (fn [[_ a b]]
           {(string->keyword a)
            (string->keyword b)}))
    first))

(defn- string->keyword
  [s]
  (or (some-> s
        str/lower-case
        str/trim
        (str/replace #"\s+" "-")
        keyword)
      :unknown))

(defmulti ->string type)

(defmethod ->string String
  [s]
  (map-rule s))

(defmethod ->string clojure.lang.PersistentArrayMap
  [m]
  (condp = (:tag m)
    :div (->string (:content m))
    :i (->string (:content m))
    :img (string->keyword (get-in m [:attrs :alt]))
    m))

(defmethod ->string clojure.lang.PersistentVector
  [v]
  (apply str (map ->string v)))

(defn- html-artist
  [parsed-html]
  (some->
    (select
      (child
        (id (format id-string "artist"))
        (sel/class "value")
        (tag :a))
      parsed-html)
    first :content first str/trim))

(defn- html-converted-mana-cost
  [parsed-html]
  (some->
    (select
      (child
        (id (format id-string "cmc"))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim
    (Integer/parseInt)))

(defn- html-expansion
  [parsed-html]
  (->>
    (map
      (fn [e]
        (when (= (-> e :content first type) java.lang.String)
          (-> e :content first)))
      (select
        (child
          (id exp-string)
          (tag :a))
        parsed-html))
    (filter identity)
    first string->keyword))

(defn- html-expansions
  [parsed-html]
  (some->>
    (select
      (child
        (id (format id-string "otherSets"))
        (sel/class "value")
        (tag "div"))
      parsed-html)
    first :content
    (map (comp string->edition :title :attrs first :content))
    (filter (complement nil?))
    (apply merge)))

(defn- html-flavor
  [parsed-html]
  (some->
    (select
      (child
        (id (format id-string "flavor"))
        (sel/class "value")
        (tag "div")
        (tag "i"))
      parsed-html)
    first :content first str/trim))

(defn- html-mana-cost
  [parsed-html]
  (some->>
    (select
      (child
        (id (format id-string "mana"))
        (sel/class "value"))
      parsed-html)
    first :content rest
    (map :attrs)
    (map :alt)
    (map
      #(if (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (first %))
        (repeat (Integer/parseInt %) "colorless")
        %))
    flatten
    (map string->keyword)
    frequencies))

(defn- html-name
  [parsed-html]
  (some->
    (select
      (child
        (id (format id-string "name"))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim))

(defn- html-power-toughness
  [parsed-html]
  (some->>
    (select
      (child
        (id (format id-string "pt"))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim
    (re-seq #"\d")
    (map #(Integer/parseInt %))))

(defn- html-rarity
  [parsed-html]
  (some->
    (select
      (child
        (id (format id-string "rarity"))
        (sel/class "value")
        (tag :span))
      parsed-html)
    first :content first str/trim string->keyword))

(defn- html-rules
  [parsed-html]
  (->>
    (select
      (child
        (id (format id-string "text"))
        (sel/class "value"))
      parsed-html)
    first :content rest))

(defn- html-types
  [parsed-html]
  (into #{}
    (some->>
      (select
        (child
          (id (format id-string "type"))
          (sel/class "value"))
        parsed-html)
      first :content first str/trim
      (re-seq #"\w+")
      (map string->keyword))))

(defn card-by-id
  [card-id]
  (let [parsed
        (->> card-id
          (format url)
          slurp
          hick/parse
          hick/as-hickory)
        ;; Used later
        power-toughness (html-power-toughness parsed)
        current-set {(html-expansion parsed)
                     (html-rarity parsed)}
        ;; Alphabetical
        artist (html-artist parsed)
        card-name (html-name parsed)
        converted-mana-cost (html-converted-mana-cost parsed)
        expansion (-> current-set first key)
        expansions (or (html-expansions parsed) current-set)
        flavor (html-flavor parsed)
        mana-cost (html-mana-cost parsed)
        power (first power-toughness)
        rarity (-> current-set first val)
        rules (map ->string (html-rules parsed))
        toughness (second power-toughness)
        types (html-types parsed)]
    {:all-sets expansions
     :artist artist
     :converted-mana-cost converted-mana-cost
     :expansion expansion
     :flavor flavor
     :mana-cost mana-cost
     :name card-name
     :power power
     :rarity rarity
     :toughness toughness
     :types types
     :rules rules}))
