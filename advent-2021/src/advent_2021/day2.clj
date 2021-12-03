(ns advent-2021.day2
  (:require [advent-2021.util :as util]
            [clojure.string :as str]))

(def start-point {:x 0 :y 0 :aim 0})

(defn make-instruction
  [x]
  (let [[direction magnitude] (str/split x #" ")]
    {:direction (keyword direction)
     :magnitude (Integer/parseInt magnitude)}))

(defn apply-instruction
  [point {:keys [direction magnitude]}]
  (case direction
    :forward (update point :x + magnitude)
    :up (update point :y - magnitude)
    :down (update point :y + magnitude)
    :else  point))

(defn apply-instruction-2
  [point {:keys [direction magnitude]}]
  (case direction
    :forward (-> point
                 (update :x + magnitude)
                 (update :y + (* magnitude (:aim point))))
    :up (update point :aim - magnitude)
    :down (update point :aim + magnitude)
    :else point))

(defn solve
  [file apply-fn]
  (->> file
       util/read-input-file
       (map make-instruction)
       (reduce apply-fn start-point)
       ((fn [{:keys [x y]}] (* x y)))))

(defn solve-1
  [file]
  (solve file apply-instruction))

(defn solve-2
  [file]
  (solve file apply-instruction-2))
