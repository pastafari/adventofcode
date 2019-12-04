(ns advent-2019.secure-container
  (:require [clojure.test :refer [deftest is]]))


(defn six-digits?
  [digits]
  (= 6 (count digits)))


(defn two-adjacent-digits-are-the-same?
  "Some two adjacent digits must be the same"
  [digits]
  (let [adjacent-digits (partition 2 1 digits)]
    (some (fn [[a b]] (= a b)) adjacent-digits)))

(defn digits-never-decrease?
  "Slightly hacky because (int \0) is actually 48, but ordering works!"
  [digits]
  (let [adjacent-digits (partition 2 1 digits)]
    (not-any? (fn [[a b]] (> (int a) (int b))) adjacent-digits)))

(defn valid-password?
  "Does the password satisfy all conditions?"
  [n]
  (let [digits (str n)]
    (and (six-digits? digits)
         (digits-never-decrease? digits)
         (two-adjacent-digits-are-the-same? digits))))

(defn count-valid-passwords
  "Of all candidate passwords, how many are valid"
  [start end]
  (loop [candidate start
         total 0]
    (if (> candidate end)
      total
      (recur
       (inc candidate)
       (if (valid-password? candidate)
         (inc total)
         total)))))

(defn solve-1
  []
  (count-valid-passwords 138241 674034))

(deftest test-valid-password?
  (is (valid-password? 111111))
  (is (not (valid-password? 223450)))
  (is (not (valid-password? 123789))))
