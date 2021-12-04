(ns advent-2021.day3
  (:require [advent-2021.util :as util]
            [clojure.string :as str]))

(defn most-common-bit
  "Most common bit in each position, 1 in case of tie"
  [xs]
  (->> xs
       frequencies
       ((fn [cs]
          (if (> (get cs 0 0) (get cs 1 0))
            0
            1)))))

(defn least-common-bit
  "Least common bit in each position, 0 in case of tie"
  [xs]
  (->> xs
       frequencies
       ((fn [cs]
          (if (> (get cs 0 0) (get cs 1 0))
            1
            0)))))

(def transpose
  (partial apply mapv vector))

(defn gamma
  "Takes a seq of xs, finds most common bit in each position"
  [xs]
  (->> xs
       transpose
       (mapv most-common-bit)))


(defn epsilon
  "Takes a seq of xs, finds least common bit in each position"
  [xs]
  (->> xs
       transpose
       (mapv least-common-bit)))

(defn read-as-ints
  [file]
  (->> file
       util/read-input-file
       (mapv (fn [s]
               (mapv #(if (= \0 %) 0 1) (vec s))))))

(defn bit-criteria-filter
  [xs criteria-fn]
  (loop [nums xs
         n 0]
    (let [mcb (criteria-fn nums)
          remaining (filterv #(= (nth % n) (nth mcb n)) nums)]
      (if (= (count remaining) 1)
        (first remaining)
        (recur remaining (inc n))))))

(defn oxygen-generator
  [xs]
  (bit-criteria-filter xs gamma))

(defn co2-scrubber
  [xs]
  (bit-criteria-filter xs epsilon))

(defn bin->dec
  "Takes a seq of 0 and 1 and returns an int"
  [xs]
  (-> xs
      str/join
      (Integer/parseInt 2)))

(defn solve-1
  [file]
  (->> file
       read-as-ints
       ((juxt gamma epsilon))
       ((fn [[g e]]
          (->> [g e]
               (mapv bin->dec)
               (apply *))))))

(defn solve-2
  [file]
  (->> file
       read-as-ints
       ((juxt oxygen-generator co2-scrubber))
       ((fn [[o c]]
          (->> [o c]
               (mapv bin->dec)
               (apply *))))))
