(ns majic.parser
  (require [majic.util :refer [string->keyword string->int]]
           [hickory.core :as hick]
           [hickory.select :as sel :refer [child id select tag]]
           [clojure.string :refer [trim join]]
           [clojure.pprint :refer [pprint]])
  (import [java.util TimerTask Timer]))

(def exp-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_currentSetSymbol")

(def exp-string-left
  ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl07_currentSetSymbol"
   "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl09_currentSetSymbol"])

(def exp-string-right
  ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl08_currentSetSymbol"
   "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl10_currentSetSymbol"])

(def id-string
  "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow")

(def id-string-left
  ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl07_%sRow"
   "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl09_%sRow"])

(def id-string-right
  ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl08_%sRow"
   "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl10_%sRow"])

(def url
  "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s")

(defn- format-id
  [id & col]
  (condp = (first col)
    :left (format id-string-left id)
    :right (format id-string-right id)
    (format id-string id)))

(defn- id-selector
  [html-id & col]
  (condp = (first col)
    :left (sel/or (id (format (first id-string-left) html-id))
                  (id (format (second id-string-left) html-id)))
    :right (sel/or (id (format (first id-string-right) html-id))
                   (id (format (second id-string-right) html-id)))
    (id (format id-string html-id))))

(defn- format-exp
  [& col]
  (condp = (first col)
    :left exp-string-left
    :right exp-string-right
    exp-string))

(defn- exp-selector
  [& col]
  (condp = (first col)
    :left (sel/or (id (first exp-string-left))
                  (id (second exp-string-left)))
    :right (sel/or (id (first exp-string-left))
                   (id (second exp-string-left)))
    (id exp-string)))

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
  (join (map ->string v)))

(defmethod ->string nil
  [n]
  nil)

(defn- html-artist
  [parsed-html & col]
  (some->
    (select
      (child
        (id-selector "artist" (first col))
        (sel/class "value")
        (tag :a))
      parsed-html)
    first :content first trim))

(defn- html-converted-mana-cost
  [parsed-html & col]
  (some->
    (select
      (child
        (id-selector "cmc" (first col))
        (sel/class "value"))
      parsed-html)
    first :content first trim
    string->int))

(defn- html-expansion
  [parsed-html & col]
  (->>
    (map
      (fn [e]
        (when (= (-> e :content first type) java.lang.String)
          (-> e :content first)))
      (select
        (child
          (exp-selector (first col))
          (tag :a))
        parsed-html))
    (filter identity)
    first string->keyword))

(defn- html-expansions
  [parsed-html & col]
  (some->>
    (select
      (child
        (id-selector "otherSets" (first col))
        (sel/class "value")
        (tag "div"))
      parsed-html)
    first :content
    (map (comp string->edition :title :attrs first :content))
    (filter (complement nil?))
    (apply merge)))

(defn- html-flavor
  [parsed-html & col]
  (some->
    (select
      (child
        (id-selector "flavor" (first col))
        (sel/class "value")
        (tag "div")
        (tag "i"))
      parsed-html)
    first :content first trim))

(defn- html-mana-cost
  [parsed-html & col]
  (some->>
    (select
      (child
        (id-selector "mana" (first col))
        (sel/class "value"))
      parsed-html)
    first :content rest
    (map :attrs)
    (map :alt)
    (map
      #(if (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (first %))
        (repeat (string->int %) "colorless")
        %))
    flatten
    (map string->keyword)
    frequencies))

(defn- html-name
  [parsed-html & col]
  (some->
    (select
      (child
        (id-selector "name" (first col))
        (sel/class "value"))
      parsed-html)
    first :content first trim))

(defn- html-power-toughness
  [parsed-html & col]
  (some->>
    (select
      (child
        (id-selector "pt" (first col))
        (sel/class "value"))
      parsed-html)
    first :content first trim
    (re-seq #"\d")
    (map string->int)))

(defn- html-rarity
  [parsed-html & col]
  (some->
    (select
      (child
        (id-selector "rarity" (first col))
        (sel/class "value")
        (tag :span))
      parsed-html)
    first :content first trim string->keyword))

(defn- html-rules
  [parsed-html & col]
  (some->>
    (select
      (child
        (id-selector "text" (first col))
        (sel/class "value"))
      parsed-html)
    first :content rest))

(defn- html-types
  [parsed-html & col]
  (set
    (some->>
      (select
        (child
          (id-selector "type" (first col))
          (sel/class "value"))
        parsed-html)
      first :content first trim
      (re-seq #"\w+")
      (map string->keyword))))

(defn- is-single?
  [parsed-html]
  (some->>
    (select
      (child
        (sel/or
          (id "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl07_rightCol")
          (id "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl09_rightCol")))
      parsed-html)
    empty?))

(defn all-card-ids
  []
  (let [url "http://gatherer.wizards.com/Pages/Search/Default.aspx?output=checklist&action=advanced&rarity=|[R]|[U]|[C]|[L]|[S]|[P]|[M]"]
    (some->> url
      slurp
      (re-seq #"\/Card\/Details.aspx\?multiverseid=(\d+)")
      (map second)
      (map string->int)
      (into #{}))))

(defn- single-card
  [parsed card-id]
  (let [power-toughness (html-power-toughness parsed)
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
        rules (remove nil? (map ->string (html-rules parsed)))
        toughness (second power-toughness)
        types (html-types parsed)]
    {:all-sets expansions
     :artist artist
     :converted-mana-cost converted-mana-cost
     :expansion expansion
     :flavor flavor
     :gatherer-id card-id
     :mana-cost mana-cost
     :name card-name
     :power power
     :rarity rarity
     :toughness toughness
     :types types
     :rules rules}))

(defn- double-card
  [parsed card-id]
  (for [col [:left :right]]
    (let [power-toughness (html-power-toughness parsed col)
          current-set {(html-expansion parsed col)
                       (html-rarity parsed col)}
          ;; Alphabetical
          artist (html-artist parsed col)
          card-name (html-name parsed col)
          converted-mana-cost (html-converted-mana-cost parsed col)
          expansion (-> current-set first key)
          expansions (or (html-expansions parsed col) current-set)
          flavor (html-flavor parsed col)
          mana-cost (html-mana-cost parsed col)
          power (first power-toughness)
          rarity (-> current-set first val)
          rules (remove nil? (map ->string (html-rules parsed col)))
          toughness (second power-toughness)
          types (html-types parsed col)]
      {:all-sets expansions
       :artist artist
       :converted-mana-cost converted-mana-cost
       :expansion expansion
       :flavor flavor
       :gatherer-id card-id
       :mana-cost mana-cost
       :name card-name
       :power power
       :rarity rarity
       :toughness toughness
       :types types
       :rules rules})))

(defn card-by-id
  [card-id]
  (let [parsed
        (->> card-id
          (format url)
          slurp
          hick/parse
          hick/as-hickory)
        is-single (is-single? parsed)]
    (if is-single
      (single-card parsed card-id)
      (double-card parsed card-id))))

(defn all-cards
  []
  (let [counter (atom 0)
        errors (atom 0)
        _ (println "Loading card id list...")
        ids (all-card-ids)
        id-count (count ids)
        _ (println "Loaded" id-count "card ids.")
        mapper (fn [id]
                 (try
                   (do
                     (swap! counter inc)
                     (card-by-id id))
                   (catch Exception e
                     (do
                       (swap! counter inc)
                       (println id "failed!")
                       nil))))
        mapper-agent (agent nil)
        logger (fn [_]
                 (loop []
                   (println @counter "/" id-count "(" @errors " errors )")
                   (Thread/sleep 10000)
                   (when (nil? @mapper-agent)
                     (recur))))
        logger-agent (agent nil)]
    (send logger-agent logger)
    (send mapper-agent (fn [_] (doall (pmap mapper ids))))
    (await mapper-agent)
    (println (count @mapper-agent) "/" @counter "parsed.")
    (println "Saving cards to file.")
    (spit "success-cards.clj" (with-out-str (pprint @mapper-agent)))
    (spit "failure-cards.clj" (with-out-str (pprint (map :gatherer-id (filter #(nil? (:name %)) @mapper-agent)))))
    (println "All is well.")
    (shutdown-agents)))