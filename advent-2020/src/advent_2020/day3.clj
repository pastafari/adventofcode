(ns advent-2020.day3
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]))

(def tree? #{\#})
(def open? #{\.})

(defn build-map
  "Takes a seq of lines of open squares and trees, and builds
  an infinitely repeating map"
  [lines]
  (into {} (map-indexed (fn [idx coll] [idx (cycle coll)])
                        lines)))

(defn point
  "Gets thing at co-ordinate x,y in area"
  [area x y]
  (when-let [xs (area y)]
    (nth xs x)))

(defn move
  "Takes an area, current position, slope and returns the value at next square"
  [area position slope]
  (let [[x y] position
        [dx dy] slope]
    (point area (+ x dx) (+ y dy))))

(defn count-trees
  "Count number of trees encountered on slope 3,1 starting at 0,0"
  [area start slope]
  (loop [count 0
         [x y] start]
    (let [next-point (move area [x y] slope)
          next-coordinate (map + [x y] slope)]
      (if (nil? next-point)
        count
        (if (tree? next-point)
          (recur (inc count)
                 next-coordinate)
          (recur count
                 next-coordinate))))))


(defn day3-1
  "Takes input file, counts trees encountered on slope 3,1"
  [file]
  (-> file
      (utils/read-input-file)
      (build-map)
      (count-trees [0 0] [3 1])))
