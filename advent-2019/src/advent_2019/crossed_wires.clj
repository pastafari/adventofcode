(ns advent-2019.crossed-wires
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def origin [0 0])

(defn right?
  [direction]
  (= \R direction))

(defn up?
  [direction]
  (= \U direction))

(defn left?
  [direction]
  (= \L direction))

(defn down?
  [direction]
  (= \D direction))


(defn vector-from-origin
  [magnitude direction]
  (cond
    (right? direction) (map vector
                            (range 1 (inc magnitude))
                            (repeat magnitude 0))
    (left? direction) (reverse (map vector
                                    (range (- magnitude) 0)
                                    (repeat magnitude 0)))
    (up? direction) (map vector
                         (repeat magnitude 0)
                         (range 1 (inc magnitude)))
    (down? direction) (reverse (map vector
                                    (repeat magnitude 0)
                                    (range (- magnitude) 0)))))

(defn instruction->points
  "Returns a vector of points that the wire would cross
  starting from position and following instruction.
  Assumes instructions have exactly one direction and one number"
  [position instruction]
  (let [[translate-x translate-y] position
        direction (first instruction)
        magnitude (Integer/parseInt (subs instruction 1))
        v (vector-from-origin magnitude direction)]
    (map (fn [[x y]] [(+ translate-x x)
                     (+ translate-y y)])
         v)))

(defn wire->points
  "We assume wires start from the origin.
  This fn transforms the instructions defining a wire
  to a vector of discrete points that the wire crosses on the grid.
  Points maybe repeated in the vector if the wire crosses over itself.
  For e.g. [R2 U1] -> [[0 0] [0 1] [1 1]]"
  [wire]
  (loop [position origin
         instructions wire
         points []]
    (if (empty? instructions)
      points
      (let [new-points (instruction->points position (first instructions))
            new-position (last new-points)]
        (recur new-position
               (rest instructions)
               (into points new-points))))))


(defn find-intersecting-points
  "Finds points that lie on both wires"
  [wire1 wire2]
  (clojure.set/intersection (set (wire->points wire1))
                            (set (wire->points wire2))))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn find-nearest-intersecting-point
  [wire1 wire2]
  (let [points (find-intersecting-points wire1 wire2)]
    (first (sort-by manhattan-distance points))))


(defn find-smallest-steps-to-intersection
  [wire1 wire2]
  (let [points1 (wire->points wire1)
        points2 (wire->points wire2)
        intersections (clojure.set/intersection (set (wire->points wire1))
                                                (set (wire->points wire2)))
        steps (for [p intersections]
                (let [i (.indexOf points1 p)
                      j (.indexOf points2 p)]
                  {:steps (+ 2 i j) :point p}))]
    (:steps (first (sort-by :steps
                            steps)))))

(defn solve-1
  [file]
  (let [[wire1 wire2] (read-wires file)]
    (apply + (find-nearest-intersecting-point wire1 wire2))))

(defn solve-2
  [file]
  (let [[wire1 wire2] (read-wires file)]
    (find-smallest-steps-to-intersection wire1 wire2)))

(defn read-wires
  "Reads in csv input from file and returns a vector of two wires
  as vectors of paths"
  [file]
  (vec
   (map #(str/split % #",")
        (-> file
            io/resource
            io/reader
            line-seq))))


(deftest test-find-shortest-manhattan-distance
  (let [wire-1 ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
        wire-2 ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]]
    (is (= 159
           (apply + (find-nearest-intersecting-point wire-1 wire-2))))
    (is (= 610
           (find-smallest-steps-to-intersection wire-1 wire-2)))))

