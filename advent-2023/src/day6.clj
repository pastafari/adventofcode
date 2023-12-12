(ns day6
  (:require [clojure.math :refer [round]]))

(defn distance
  "Returns distance when hold time is h and max time is t
  Observe that this is a parabolic curve, with symmetry.
  We need not compute all values. Just the maxima, and a few points to the left
  or right until we get to the threshold.
  Maxima will always be at t/2! For t #{I}, odd t will have
  two maxima at floor(t/2) and ceil(t/2).
  Even t will have exactly one."
  [t h]
  (* h (- t h)))

(defn count-winning-hold-times?
  "Given total time t, counts how many hold times are greater than winning-time.
  Uses the idea that distance(h) == distance(t-h), and maxima is always at t/2"
  [t winning-distance]
  ;; round rounds upward.
  (let [maxima (round (/ t 2))
        winner? #(> (distance t %)
                    winning-distance)
        wins (take-while winner?
                         (range maxima t))
        w-count (count wins)]
    (if (even? t)
      (dec (* 2 w-count))
      (* 2 w-count))))
