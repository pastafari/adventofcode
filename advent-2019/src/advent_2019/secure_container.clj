(ns advent-2019.secure-container
  (:require [clojure.test :refer [deftest is]]))


(defn six-digits?
  [digits]
  (= 6 (count digits)))


(defn adjacent-digits
  [digits]
  (partition 2 1 digits))

(defn two-adjacent-digits-are-the-same?
  "Some two adjacent digits must be the same"
  [digits]
  (some (fn [[a b]] (= a b)) (adjacent-digits digits)))

(defn digits-never-decrease?
  "Slightly hacky because (int \0) is actually 48, but ordering works!"
  [digits]
  (not-any? (fn [[a b]] (> (int a) (int b))) (adjacent-digits digits)))

(defn same-adjacent-digits-not-part-of-larger-group?
  "two adjacent matching digits are not part of a larger group of matching digits"
  [digits]
  (let [adjacent-pairs (keep (fn [[a b]] (when (= a b) [a b]))
                             (adjacent-digits digits))]
    (some (fn [[k v]] (= v 1)) (frequencies adjacent-pairs))))

(defn valid-password-1?
  "Does the password satisfy all conditions?"
  [n]
  (let [digits (str n)]
    (and (six-digits? digits)
         (digits-never-decrease? digits)
         (two-adjacent-digits-are-the-same? digits))))

(defn valid-password-2?
  "Adds a new condition to valid-password-1?"
  [n]
  (and (valid-password-1? n)
       (same-adjacent-digits-not-part-of-larger-group? (str n))))

(defn count-valid-passwords
  "Of all candidate passwords, how many are valid"
  [start end valid-password-fn]
  (loop [candidate start
         total 0]
    (if (> candidate end)
      total
      (recur
       (inc candidate)
       (if (valid-password-fn candidate)
         (inc total)
         total)))))

(defn solve-1
  []
  (count-valid-passwords 138241 674034 valid-password-1?))

(defn solve-2
  []
  (count-valid-passwords 138241 674034 valid-password-2?))

(deftest test-valid-password-1?
  (is (valid-password-1? 111111))
  (is (not (valid-password-1? 223450)))
  (is (not (valid-password-1? 123789))))

(deftest test-valid-password-2?
  (is (valid-password-2? 112233))
  (is (not (valid-password-2? 123444)))
  (is (valid-password-2? 111122)))
