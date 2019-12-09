(ns advent-2019.sensor-boost
  (:require [advent-2019.sunny-with-a-chance-of-asteroids :as intcode]))

(defn solve-1
  [file]
  (-> file
      intcode/read-program
      (intcode/eval-program {})))
