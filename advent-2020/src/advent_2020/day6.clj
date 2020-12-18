(ns advent-2020.day6
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]))

(defn everyone-answered-count
  "Takes an answer group, counts answers that everyone gave
by making set intersection"
  [coll]
  (->> coll
       (map #(into #{} %))
       (apply clojure.set/intersection)
       count))

(defn unique-answer-count
  "Takes an answer group, counts unique answers by making set union"
  [coll]
  (->> coll
       (map #(into #{} %))
       (apply clojure.set/union)
       count))


(defn make-answer-groups
  "Each group's answers are separated by a blank line,
  and within each group, each person's answers are on a single line.
  Returns a seq of groups, each group is a seq of answers"
  [lines]
  (->> lines
       (partition-by #{""})
       (filter #(not (= '("") %)))))


(defn day6-1
  "For each group, count the number of questions to which anyone answered yes,
  What is the sum of those counts?"
  [file]
  (->> file
       utils/read-input-file
       make-answer-groups
       (map unique-answer-count)
       (apply +)))

(defn day6-2
  "For each group, count the number of questions to which everyone answered Yes.
  What is the sum of those counts?"
  [file]
  (->> file
       utils/read-input-file
       make-answer-groups
       (map everyone-answered-count)
       (apply +)))
