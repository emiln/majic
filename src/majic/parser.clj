(ns majic.parser
  (require [majic.util :refer [n-partitions string->keyword string->int]]
           [hickory.core :as hick]
           [hickory.select :as sel :refer [child id select tag]]
           [org.httpkit.client :as c]
           [clojure.string :refer [trim join]]
           [clojure.pprint :refer [pprint]]
           [clojure.core.async :as async :refer [alts! chan close! go go-loop
             map< map> onto-chan put! timeout <! <!! >! >!!]])
  (import [java.util TimerTask Timer]))

(def ^{:private true}
  lookups
  "A map containing a lot of ugly strings related to HTML IDs and URLs."
  {:exp-string
    "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_currentSetSymbol"
   :exp-string-left
    ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl07_currentSetSymbol"
     "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl09_currentSetSymbol"]
   :exp-string-right
    ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl08_currentSetSymbol"
     "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl10_currentSetSymbol"]
   :id-string
    "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_%sRow"
   :id-string-left
    ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl07_%sRow"
     "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl09_%sRow"]
   :id-string-right
    ["ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl08_%sRow"
     "ctl00_ctl00_ctl00_MainContent_SubContent_SubContent_ctl10_%sRow"]
   :url
    "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%s"
   :all-cards-url
    (str "http://gatherer.wizards.com/Pages/Search/Default.aspx?output=checkli"
      "st&action=advanced&rarity=%7C[R]%7C[U]%7C[C]%7C[L]%7C[S]%7C[P]%7C[M]")})

(defn- format-id
  [id & col]
  (condp = (first col)
    :left (format (:id-string-left lookups) id)
    :right (format (:id-string-right lookups) id)
    (format (:id-string lookups) id)))

(defn- id-selector
  [html-id & col]
  (condp = (first col)
    :left (sel/or (id (format (first (:id-string-left lookups)) html-id))
                  (id (format (second (:id-string-left lookups)) html-id)))
    :right (sel/or (id (format (first (:id-string-right lookups)) html-id))
                   (id (format (second (:id-string-right lookups)) html-id)))
    (id (format (:id-string lookups) html-id))))

(defn- format-exp
  [& col]
  (condp = (first col)
    :left (:exp-string-left lookups)
    :right (:exp-string-right lookups)
    (:exp-string lookups)))

(defn- exp-selector
  [& col]
  (condp = (first col)
    :left (sel/or (id (first (:exp-string-left lookups)))
                  (id (second (:exp-string-left lookups))))
    :right (sel/or (id (first (:exp-string-right lookups)))
                   (id (second (:exp-string-right lookups))))
    (id (:exp-string lookups))))

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

(defmulti ^{:private true}
  ->string type)

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

(defn- html-gatherer-id
  [parsed-html]
  (some->>
    (select
      (sel/tag "form")
      parsed-html)
    first :attrs :action
    (re-seq #"\d+")
    first
    string->int))

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
  "Returns a channel onto which all card IDs will eventually be put.
  
   Be mindful that fetching all card IDs may take several minutes depending on
   things like Gatherer's current mood, your internet connection, and the
   disposition of various Ethernet gods of old."
  []
  (let [in (chan)
        out (chan)]
    ;(async-get (:all-cards-url lookups) in)
    (go (>! in (slurp (:all-cards-url lookups))))
    (go
      (some->> (<! in)
        (re-seq #"\/Card\/Details.aspx\?multiverseid=(\d+)")
        (map second)
        (map string->int)
        (into #{})
        (onto-chan out)))
    out))

(defn- single-card
  [parsed]
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
        gatherer-id (html-gatherer-id parsed)
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
     :gatherer-id gatherer-id
     :mana-cost mana-cost
     :name card-name
     :power power
     :rarity rarity
     :toughness toughness
     :types types
     :rules rules}))

(defn- double-card
  [parsed]
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
          gatherer-id (html-gatherer-id parsed)
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
       :gatherer-id gatherer-id
       :mana-cost mana-cost
       :name card-name
       :power power
       :rarity rarity
       :toughness toughness
       :types types
       :rules rules})))

(defn card-by-id
  "Looks up the given card ID in Gatherer and returns the parsed card."
  [card-id]
  (let [parsed
        (->> card-id
          (format (:url lookups))
          slurp
          hick/parse
          hick/as-hickory)
        is-single (is-single? parsed)]
    (if is-single
      (single-card parsed)
      (double-card parsed))))

(defn- card-url
  [id]
  (format (:url lookups) id))

(defn- parse-card
  [html-string]
  (let [parsed (some->> html-string
                 hick/parse
                 hick/as-hickory)]   
    ((if (is-single? parsed) single-card double-card)
     parsed)))

(defn- async-get
  [url channel rate-limit]
  (<!! rate-limit)
  (c/get url {:timeout 3600000}
    (fn [{:keys [status headers body error]}]
      (>!! rate-limit :ready)
      (when-not error
        (put! channel body)))))

(defn all-cards
  "Returns a channel onto which all cards from the Gatherer database will be
   put.

   Be aware that although the channel is returned immediately, it will take a
   minute or so before any cards are actually put onto the channel.

   The following example processes all cards by printing each card name:

   (require '[majic.parser :refer [all-cards]])
   (require '[clojure.core.async :refer [<! go-loop]])
   (let [channel (all-cards)]
     (go-loop [counter 1]
       (when-let [card (<! channel)]
         (println (format \"%05d: %s\" counter
                          (or (:name card)
                              (str (:name (first card))
                                   \" // \"
                                   (:name (second card))))))
         (recur (inc counter)))))"
  []
  (let [limit 100
        id-chan (all-card-ids)
        raw-chan (chan)
        rate-limit (chan limit)]
    (dotimes [i limit]
      (>!! rate-limit :ready))
    (go-loop []
      (when-let [id (<! id-chan)]
        (async-get (card-url id) raw-chan rate-limit)
        (recur)))
    (map< #(parse-card %) raw-chan)))
