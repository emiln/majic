(ns majic.parser
  (require [hickory.core :as hick]
           [hickory.select :as sel]
           [clojure.string :as str]))

(def url
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s")

(def id-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow")

(defn- html-artist
  [id parsed-html]
  (->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag :a))
      parsed-html)
    first :content first str/trim))

(defn- html-name
  [id parsed-html]
  (->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value"))
      parsed-html)
    first :content first str/trim))

(defn- html-expansion
  [id parsed-html]
  (->
    (sel/select
      (sel/child
        (sel/id (format id-string id))
        (sel/class "value")
        (sel/tag :div)
        (sel/nth-child 2))
      parsed-html)
    first :content first str/trim))

(defn- html-rarity
  [id parsed-html]
  (->
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
     :rarity
     (html-rarity "rarity" parsed)
     :rules
     (map
       (fn [elem] elem)
       (html-mana-cost "text" parsed))}))