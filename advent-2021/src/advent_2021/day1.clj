(ns advent-2021.day1
  (:require [advent-2021.util :as util]))

(defn count-increases
  [xs]
  (->> xs
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))

(defn count-sliding-increases
  [xs]
  (->> xs
       (partition 3 1)
       (map (partial apply +))
       count-increases))

(defn solve
  "Reads file as int seq and counts increases"
  [file f]
  (->> file
       util/read-input-file
       (map #(Integer/parseInt %))
       f))

(def solve-1
  (solve "day1.input" count-increases))


(def solve-2
  (solve "day1.input" count-sliding-increases))
