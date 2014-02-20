(ns majic.parser
  (require [hickory.core :as hick]
           [hickory.select :as sel]
           [clojure.string :as str]))

(def url
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s")

(def id-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow")

(def exp-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_currentSetSymbol")

(defn- html-artist
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag :a))
      parsed-html)
    first :content first str/trim))

(defn- html-name
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim))

(defn- html-flavor
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag "div")
        (sel/tag "i"))
      parsed-html)
    first :content first str/trim))

(defn- html-power-toughness
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim
    (#(re-seq #"\d" %))))
(defn- html-expansion
  [id parsed-html]
  (->>
    (map
      (fn [e]
        (when (= (-> e :content first type) java.lang.String)
          (-> e :content first)))
      (sel/select
        (sel/child
          (sel/id exp-string)
          (sel/tag :a))
        parsed-html))
    (filter identity)))

(defn- html-expansions
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag "div"))
      parsed-html)
    first :content
    (#(map (comp :title :attrs first :content) %))
    (#(filter (complement nil?) %))))

(defn- html-rarity
  [id parsed-html]
  (some->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag :span))
      parsed-html)
    first :content first str/trim))

(defn- html-mana-cost
  [id parsed-html]
  (->>
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value"))
      parsed-html)
    first :content
    (drop 1)))

(defn card-by-id
  [id]
  (let [parsed
        (->> id
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
     (map
       (fn [elem]
         (-> elem
           :attrs :alt))
       (html-mana-cost "mana" parsed))
     :converted-mana-cost
     (html-name "cmc" parsed)
     :types
     (html-name "type" parsed)
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
       (html-mana-cost "text" parsed))}))
