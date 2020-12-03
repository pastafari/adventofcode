(ns advent-2020.day2
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]))

(defn parse-rule-and-password
  "Takes a string of the form
  1-3 a: xyxxaxxax
  and returns a map with {:password xyxxaxxax :rule {:char-str a :n1 1 :n2 3}}"
  [s]
  (let [[rule password] (str/split s #":")
        [min-max char] (str/split rule #" ")
        [min max] (str/split min-max #"-")]
    {:password (str/trim password)
     :rule {:char-str char
            :n1 (Integer/parseInt min)
            :n2 (Integer/parseInt max)}}))

(defn min-rule?
  "Does char occur at least min times in password?"
  [{:keys [rule password]}]
  (let [min (:n1 rule)
        char (.charAt (:char-str rule) 0)
        occurences (keep #{char} (seq password))]
    (>= (count occurences) min)))


(defn max-rule?
  "Does char occur at most max times in password?"
  [{:keys [rule password]}]
  (let [max (:n2 rule)
        char (.charAt (:char-str rule) 0)
        occurences (keep #{char} (seq password))]
    (<= (count occurences) max)))


(defn valid-password-min-max?
  "Takes a string of the form 1-3 a: xyxxaxxax and returns true iff password
is valid"
  [s]
  (let [rule-and-password (parse-rule-and-password s)]
    (and (min-rule? rule-and-password)
         (max-rule? rule-and-password))))


(defn day2-1
  "Reads input, returns count of valid passwords"
  [file]
  (let [rules-and-passwords (utils/read-input-file file)]
    (count (filter valid-password-min-max? rules-and-passwords))))


(deftest test-valid-password-min-max?
  (testing "it is valid if all conditions met"
    (is (true? (valid-password-min-max? "1-3 a: abcde")))
    (is (true? (valid-password-min-max? "2-9 c: ccccccccc")))
    (is (true? (valid-password-min-max? "3-7 g: gdgtnfggq"))))
  (testing "it is not valid if some conditions not met"
    (is (false? (valid-password-min-max? "1-3 b: cdefg")))))
