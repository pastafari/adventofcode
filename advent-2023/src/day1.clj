(ns day1
  (:require [util :as u]))

(def re-1 #"\d")

(def re-2 #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))")

(defn str->num
  [s]
  (let [nums {"one" "1"
              "two" "2"
              "three" "3"
              "four" "4"
              "five" "5"
              "six" "6"
              "seven" "7"
              "eight" "8"
              "nine" "9"}]
    (or (get nums s) s)))

(defn line->number
  "Reads a line like pqr3stu8vwx and combines the first digit and last digit to
make a 2 digit number"
  [re line]
  (let [ms (re-seq re line)
        f (first ms)
        l (last ms)
        ;; re-1 leads to a re-seq with strings in it!
        ;; re-2 leads to a re-seq with vectors in it!
        get-num-str (fn [x] (if (vector? x) (last x) x))]
    (if (and f l)
      (Integer/parseInt (str (str->num (get-num-str f))
                             (str->num (get-num-str l))))
      0)))

(defn solve
  "Reads \"in\", parses numbers, sums them up"
  [in re]
  (->> in
       u/input-seq
       (map (partial line->number re))
       (apply +)))

(comment
  ;; part 1
  (solve "day1.input" re-1)
  ;; part 2
  (solve "day1.input" re-2))
