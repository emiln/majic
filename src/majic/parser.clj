(ns majic.parser
  (require [hickory.core :as hick]
           [hickory.select :as sel :refer [child id select tag]]
           [clojure.string :as str]))

(def url
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s")

(def id-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow")

(def exp-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_currentSetSymbol")

(defn- string->keyword
  [s]
  (or (some-> s
        str/lower-case
        str/trim
        (str/replace #"\s+" "-")
        keyword)
      :unknown))

(defn- string->edition
  [s]
  (some->> s
    (re-seq #"([\w\s]+)\(([\w\s]+)\)")
    (map (fn [[_ a b]]
           {(string->keyword a)
            (string->keyword b)}))
    first))

(defn- html-artist
  [html-id parsed-html]
  (some->
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value")
        (tag :a))
      parsed-html)
    first :content first str/trim))

(defn- html-name
  [html-id parsed-html]
  (some->
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim))

(defn- html-converted-mana-cost
  [html-id parsed-html]
  (some->
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim
    (Integer/parseInt)))

(defn- html-types
  [html-id parsed-html]
  (into #{}
    (some->>
      (select
        (child
          (id (format id-string html-id))
          (sel/class "value"))
        parsed-html)
      first :content first str/trim
      (re-seq #"\w+")
      (map string->keyword))))

(defn- html-flavor
  [html-id parsed-html]
  (some->
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value")
        (tag "div")
        (tag "i"))
      parsed-html)
    first :content first str/trim))

(defn- html-power-toughness
  [html-id parsed-html]
  (some->>
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim
    (re-seq #"\d")
    (map #(Integer/parseInt %))))

(defn- html-expansion
  [html-id parsed-html]
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
  [html-id parsed-html]
  (some->>
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value")
        (tag "div"))
      parsed-html)
    first :content
    (map (comp string->edition :title :attrs first :content))
    (filter (complement nil?))
    (apply merge)))

(defn- html-rarity
  [html-id parsed-html]
  (some->
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value")
        (tag :span))
      parsed-html)
    first :content first str/trim string->keyword))

(defn- html-mana-cost
  [html-id parsed-html]
  (some->>
    (select
      (child
        (id (format id-string html-id))
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

(defn- html-rules
  [html-id parsed-html]
  (->>
    (select
      (child
        (id (format id-string html-id))
        (sel/class "value"))
      parsed-html)
    first :content rest))

(defn card-by-id
  [card-id]
  (let [parsed
        (->> card-id
          (format url)
          slurp
          hick/parse
          hick/as-hickory)]
    {:artist
     (html-artist "artist" parsed)
     :flavor
     (html-flavor "flavor" parsed)
     :name
     (html-name "name" parsed)
     :mana-cost
     (html-mana-cost "mana" parsed)
     :converted-mana-cost
     (html-converted-mana-cost "cmc" parsed)
     :types
     (html-types "type" parsed)
     :expansion
     (html-expansion "set" parsed)
     :all-sets
     (html-expansions "otherSets" parsed)
     :power (first (html-power-toughness "pt" parsed))
     :toughness (second (html-power-toughness "pt" parsed))
     :rarity
     (html-rarity "rarity" parsed)
     :rules
     (map
       (fn [elem] (:content elem))
       (html-rules "text" parsed))}))
