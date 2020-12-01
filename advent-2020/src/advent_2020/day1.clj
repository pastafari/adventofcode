(ns advent-2020.day1
  (:require [advent-2020.utils :as utils]
            [clojure.test :refer [deftest testing is]]))

(defn find-pair-sum
  "Finds a pair of distinct numbers from coll that sum to n, if none returns nil"
  [coll n]
  (let [input-set (into #{} coll)
        has-pair? (fn [i] (input-set (- n i)))
        p2 (some has-pair? coll)]
    (when p2
      (into #{} [(- n p2) p2]))))


(defn find-triplet-sum
  "Finds three distinct numbers from coll that sum up to n"
  [coll n]
  (let [input-set (into #{} coll)
        has-pair? (fn [i] (find-pair-sum (disj input-set i)
                                        (- n i)))
        triples (keep identity
                      (map (fn [i] (when-let [p (has-pair? i)]
                                    (conj p i)))
                           coll))]
    (first triples)))

(defn day1-1
  "Reads inputs from file, finds pair that sum to 2020, returns their product"
  [file]
  (let [n 2020
        str-coll (utils/read-input-file file)
        int-coll (map (fn [x] (Integer/parseInt x)) str-coll)
        pair (find-pair-sum int-coll n)]
    (apply * pair)))


(defn day1-2
  "Reads inputs from file, finds triplet that sum to 2020, returns their product"
  [file]
  (let [n 2020
        str-coll (utils/read-input-file file)
        int-coll (map (fn [x] (Integer/parseInt x)) str-coll)
        triplet (find-triplet-sum int-coll n)]
    (apply * triplet)))

(deftest test-find-pair-sum
  (testing "it finds the two numbers that sum up to n"
    (is (= #{1721 299}
           (find-pair-sum [1721
                           979
                           366
                           299
                           675
                           1456]
                          2020)))))

(deftest test-find-triplet-sum
  (testing "it finds three numbers that sum up to n"
    (is (= #{979 366 675}
           (find-triplet-sum [1721
                              979
                              366
                              299
                              675
                              1456]
                             2020)))))
