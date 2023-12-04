(ns day2
  (:require [clojure.string :as s]
            [util :as u]))

(defn power
  [{:strs [red green blue]}]
  (* red green blue))

(defn min-cubes-for-possible-game
  "Returns a map of counts of colors that are required to make a game possible"
  [game]
  (apply merge-with max (:revealed game)))

(defn possible-reveal?
  [available-colors reveal]
  (let [possibles? (map (fn [[color cnt]]
                          (<= cnt (get available-colors color 0)))
                        reveal)]
    (every? true? possibles?)))

(defn possible-game?
  "Given totals of all available colors, returns true iff
  all colors revealed in a game are possible"
  [available-colors {:keys [revealed]}]
  (let [p-games? (map #(possible-reveal? available-colors %)
                      revealed)]
    (every? true? p-games?)))

(defn line->game
  "Converts a line input to a map"
  [line]
  (let [[game reveals] (s/split line #":")
        id (Integer/parseInt (re-find #"\d+" game))
        revealed-seq (s/split reveals #";")
        revealed (reduce (fn [acc rs]
                           (let [r (for [[_ num color] (re-seq #"(\d+) ([a-z]*)" rs)]
                                     [color (Integer/parseInt num)])]
                             (conj acc (into {} r))))
                         []
                         revealed-seq)]
    {:id id
     :revealed revealed}))

(defn solve
  [in available-colors]
  (->> in
       u/input-seq
       (map line->game)
       (filter (partial possible-game? available-colors))
       (map (fn [{:keys [id]}] (println id) id))
       (apply +)))

(defn solve-2
  [in]
  (->> in
       u/input-seq
       (map line->game)
       (map min-cubes-for-possible-game)
       (map power)
       (apply +)))

(comment
  (solve "day2.input" {"red" 12
                       "green" 13
                       "blue" 14}))
