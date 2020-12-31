(ns advent-2020.day7
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [ubergraph.core :as graph]
            [ubergraph.alg :as alg]))


(defn str->color
  "takes a color string of the form 'vibrant maroon', returns a keyword of the form
  :vibrant-maroon"
  [s]
  (keyword (str/replace s #"\s" "-")))

(defn contain-rule->vec
  "Transforms a rule like '3 wavy black bags' into a vector
  [:wavy-black 3], returns nil if no other bags"
  [rule]
  (let [[_ n color] (re-find #"(no|\d+) ([a-z\s]*) bags?" rule)]
    (when (not (= "no" n))
      [(str->color color)
       (Integer/parseInt n)])))


(defn parse-rule
  "Parses a rule of the form
  vibrant maroon bags contain 5 vibrant lavender bags, 3 wavy black bags, 2 striped magenta bags, 2 pale green bags.
  into a map representing the relationship between bags
  map keys {:bag :vibrant-maroon :contains [[:vibrant-lavender 5]
                                            [:wavy-black 3]
                                            [:striped-magenta 2]
                                            [:pale-green 2]]}"
  [rule]
  (let [[_ bag-str contains-str] (re-find #"([a-z\s]*) bags contain (.*)\."
                                         rule)
        bag (str->color bag-str)
        contains (remove nil? (mapv contain-rule->vec (str/split contains-str #",")))]
    {:bag bag
     :contains contains}))

(defn rule->edges
  "Takes a rule and builds edges of a weighted digraph"
  [{:keys [bag contains]}]
  (map (fn [[color n]] [color bag n]) contains))

(defn build-graph
  "Takes rules and builds a weighted digraph representing them,
  an edge represents the fact that a bag contains n other bags pointing from containee to container"
  [rules]
  (let [edges (mapcat rule->edges rules)]
    (apply graph/digraph edges)))

(defn rule->edges-1
  [{:keys [bag contains]}]
  (map (fn [[color n]] [bag color n]) contains))

(defn build-graph-1
  "Takes rules and builds a weighted digraph representing them,
  an edge represents the fact that a bag contains n other bags pointing from containee to container"
  [rules]
  (let [edges (mapcat rule->edges-1 rules)]
    (apply graph/digraph edges)))

(defn count-bags-that-can-contain
  "Finds all bags reachable from given color"
  [color graph]
  (let [start-node (alg/shortest-path graph {:start-node color})]
    ;; decrement count because the start node is included in the result.
    (dec (count (alg/all-destinations start-node)))))

(defn count-bags-contained-in
  "Counts cumulative weight of all nodes reachable from given color"
  [color graph]
  (let [edges color]
    ))

(defn day7-1
  "How many bag colors can eventually contain at least one shiny gold bag?"
  [file]
  (->> file
       utils/read-input-file
       (map parse-rule)
       build-graph
       (count-bags-that-can-contain :shiny-gold)))

(defn day7-2
  "How many individual bags are required inside your single shiny gold bag?"
  [file]
  (->> file
       utils/read-input-file
       (map parse-rule)
       build-graph-1
       (count-bags-contained-in :shiny-gold)))
