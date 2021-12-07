(ns advent-2021.day7
  (:require [advent-2021.util :as util]))

(defn fuel-fn-2
  "Summation of 1 to n-steps, using formula!"
  [x]
  (fn [m] (let [n (Math/abs (- m x))]
           (/ (* n (inc n)) 2))))

(defn fuel-fn-1
  [x]
  (fn [n] (Math/abs (- n x))))

(defn calculate-fuel
  [ns x fuel-fn]
  (apply +
         (mapv (fuel-fn x) ns)))

(defn minimize-fuel
  [ns fuel-fn]
  (let [m1 (apply min ns)
        m2 (apply max ns)
        costs (mapv #(calculate-fuel ns % fuel-fn) (range m1 (inc m2)))
        [index cost] (apply min-key second (map-indexed vector costs))]
    [index cost]))

(defn solve
  [file fuel-fn]
  (let [ns (->> file
              util/read-input-file
              (mapv util/csv->ints)
              first)]
    (minimize-fuel ns fuel-fn)))

(defn solve-1
  [file]
  (solve file fuel-fn-1))

(defn solve-2
  [file]
  (solve file fuel-fn-2))
