(ns advent-2021.day6
  (:require [advent-2021.util :as util]
            [clojure.string :as str]))

(defn simulate-lanternfish
  "Takes a map of timers to count"
  [timer-counts]
  (let [new-fish-count (get timer-counts 0 0)]
    {0 (get timer-counts 1 0)
     1 (get timer-counts 2 0)
     2 (get timer-counts 3 0)
     3 (get timer-counts 4 0)
     4 (get timer-counts 5 0)
     5 (get timer-counts 6 0)
     6 (+ new-fish-count (get timer-counts 7 0))
     7 (get timer-counts 8 0)
     8 new-fish-count}))

(defn parse-timers
  [x]
  (mapv #(Integer/parseInt %)
        (str/split x #",")))


(defn solve
  "Gets count after n days"
  [file n]
  (let [sims (->> file
                  util/read-input-file
                  ;; because read returns a line-seq
                  first
                  parse-timers
                  frequencies
                  (iterate simulate-lanternfish))]
    (apply + (vals (nth sims n)))))
