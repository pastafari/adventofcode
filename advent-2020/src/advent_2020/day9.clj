(ns advent-2020.day9
  (:require [advent-2020.utils :as utils]))

(defn contains-two-numbers-summing-to?
  "Returns true iff coll contains at least two distinct numbers summing to n"
  [coll n]
  (let [coll-set (into #{} coll)]
    (boolean (some #(and (coll-set (- n %))
                         (not= % (- n %)))
                   coll))))


(defn find-first-number-not-sum-of-previous-25
  "Finds the first number that is not the sum of two of the 25 before it"
  [coll]
  (let [candidates (drop 25 coll)
        not-xmas-numbers (map-indexed (fn [idx c]
                                        (when-not
                                            (contains-two-numbers-summing-to?
                                             (subvec coll idx (+ 25 idx))
                                             c)
                                            c))
                                      candidates)]
    (some identity not-xmas-numbers)))

(defn day9-1
  "Reads input, finds find the first number in the list (after the preamble)
  which is not the sum of two of the 25 numbers before it."
  [file]
  (->> file
       utils/read-input-file
       (mapv #(Long/parseLong %))
       find-first-number-not-sum-of-previous-25))

;; TODO: part 2 is contiguous subsequence that sums to N. Do it some day when
;; you're feeling fresh and rested
