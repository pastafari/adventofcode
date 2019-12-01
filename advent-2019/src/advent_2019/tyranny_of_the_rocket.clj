(ns advent-2019.tyranny-of-the-rocket
  (:require [clojure.java.io :as io]))

(defn fuel-required-for-module
  "Calculates fuel required given m - the mass of the module.
  Fuel itself contributes to mass, so additional fuel is required
  each time fuel is computed for a given mass to account for the mass
  of the fuel itself!
  Example:
  At first, a module of mass 1969 requires 654 fuel.
  Then, this fuel requires 216 more fuel (654 / 3 - 2).
  216 then requires 70 more fuel, which requires 21 fuel,
  which requires 5 fuel, which requires no further fuel.
  So, the total fuel required for a module of mass 1969 is
  654 + 216 + 70 + 21 + 5 = 966.
  "
  [m]
  (letfn [(fuel-for-mass [mass]
            (int (- (Math/floor (/ mass 3)) 2)))]
    (loop [fuel 0
           mass m]
      (let [additional-fuel (fuel-for-mass mass)]
        (if (not (pos? additional-fuel))
          fuel
          (recur (+ fuel additional-fuel)
                 additional-fuel))))))

(defn total-fuel-required
  [modules]
  (apply + (map fuel-required-for-module modules)))

(defn read-modules
  "Reads lines from file and converts to integers representing mass"
  [file]
  (map #(Integer/parseInt %)
       (line-seq (io/reader (io/resource file)))))

(defn solve-1
  "Reads inputs from file and calculates total fuel required"
  [file]
  (-> file
      read-modules
      total-fuel-required))
