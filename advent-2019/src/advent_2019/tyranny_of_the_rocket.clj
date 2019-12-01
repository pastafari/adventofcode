(ns advent-2019.tyranny-of-the-rocket
  (:require [clojure.java.io :as io]))

(defn total-fuel-required
  [modules]
  (apply + (map fuel-required modules)))

(defn fuel-required
  [module]
  (let [m (:mass module)]
    (int (- (Math/floor (/ m 3)) 2))))


(defn build-modules
  "A module is a map with keys :mass"
  [m]
  {:mass (Integer/parseInt m)})


(defn read-modules
  "Reads lines from file and converts to a module represented by
  {:mass m}"
  [file]
  (map build-modules
       (line-seq (io/reader (io/resource file)))))

(defn solve-1
  "Reads inputs from file and calculates total fuel required"
  [file]
  (-> file
      read-modules
      total-fuel-required))
