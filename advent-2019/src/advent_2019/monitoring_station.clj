(ns advent-2019.monitoring-station
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.java.io :as io]))


(defn squared-distance
  "Square of distance between two asteroids"
  [[x1 y1] [x2 y2]]
  (+ (* (- x2 x1) (- x2 x1))
     (* (- y2 y1) (- y2 y1))))

(defn is-between?
  "Is asteroid c between a and b?
  Yes iff cross-product (b-a) x (c-a) is 0
  AND dot-product (b-a).(c-a) > 0
  AND squared-distance(a,b) < dot-product"
  [a b c]
  (let [[x1 y1] a
        [x2 y2] b
        [x3 y3] c
        cross-product (- (* (- y3 y1) (- x2 x1))
                         (* (- x3 x1) (- y2 y1)))
        dot-product (+ (* (- x3 x1) (- x2 x1))
                       (* (- y3 y1) (- y2 y1)))
        squared-distance-a-b (squared-distance a b)]
    (and (zero? cross-product)
         (pos? dot-product)
         (> squared-distance-a-b dot-product))))


(defn get-visible-asteroids
  "Counts asteroids that are directly visible from p,
  i.e. no asteroid in between p and the other asteroid"
  [p asteroids]
  (let [other-asteroids (disj asteroids p)
        line-of-sight (keep (fn [other]
                              (let [remaining (disj other-asteroids other)]
                                (when (not-any? #(is-between? p other %)
                                                remaining)
                                  other)))
                            other-asteroids)]
    line-of-sight))

(defn count-visible-asteroids
  [p asteroids]
  {:count (count (get-visible-asteroids p asteroids))
   :asteroid p})

(defn read-asteroids
  "Reads file and returns asteroids as a set of points in the x-y plane"
  [file]
  (let [lines (-> file
                  io/resource
                  io/file
                  io/reader
                  line-seq)]
    ;; acrobatics to build a set of asteroids!
    (into #{} (mapcat identity
                      (map-indexed (fn [y line]
                                     (remove nil? (map-indexed
                                                   (fn [x p] (when (= p \#) [x y]))
                                                   line)))
                                   lines)))))

(defn angle
  "Gets the angle between two points in the plane in degrees modulo 360"
  [[x1 y1] [x2 y2]]
  (let [deg (Math/toDegrees (+ (Math/atan2 (- y2 y1) (- x2 x1))
                               (/ Math/PI 2)))]
    (mod deg 360)))

(defn solve-1
  [file]
  (let [asteroids (read-asteroids file)
        visible (map (fn [a] (count-visible-asteroids a asteroids))
                     asteroids)]    
    (apply max-key :count visible)))

(defn solve-2
  [file a]
  (let [asteroids (read-asteroids file)
        visible (get-visible-asteroids a asteroids)
        angles-and-distances (map (fn [v] {:angle (angle a v)
                                          :distance (squared-distance a v)
                                          :asteroid v})
                                  visible)
        sorted (sort-by (juxt :angle :distance) angles-and-distances)]
    (nth sorted 199)))

(deftest test-is-between?
  (is (is-between? [1 0] [4 0] [3 0]))
  (is (not (is-between? [1 0] [3 0] [4 0])))
  (is (is-between? [1 0] [3 4] [2 2])))


(deftest test-count-visible-asteroids
  (let [asteroids (read-asteroids "monitoring_station/test-input-1")]
    (is (= 7 (:count (count-visible-asteroids [1 0] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [4 0] asteroids))))
    (is (= 6 (:count (count-visible-asteroids [0 2] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [1 2] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [2 2] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [3 2] asteroids))))
    (is (= 5 (:count (count-visible-asteroids [4 2] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [4 3] asteroids))))
    (is (= 8 (:count (count-visible-asteroids [3 4] asteroids))))
    (is (= 7 (:count (count-visible-asteroids [4 4] asteroids))))))

(deftest test-solve-1
  (is (= 8 (solve-1 "monitoring_station/test-input-1")))
  (is (= 33 (solve-1 "monitoring_station/test-input-2")))
  (is (= 35 (solve-1 "monitoring_station/test-input-3")))
  (is (= 41 (solve-1 "monitoring_station/test-input-4")))
  (is (= 210 (solve-1 "monitoring_station/test-input-5"))))
