(ns advent-2021.day5
  (:require [advent-2021.util :as util]
            [clojure.string :as str]))

(defn str->point
  "Takes a point 0,9 returns a vec [0 9]"
  [s]
  (mapv #(Integer/parseInt %)
        (str/split s #",")))

(defn parse-line
  "Takes an input line like x1,y1 -> x2,y2 returns a pair of points [[x1 y1] [x2 y2]]"
  [in]
  (let [points (mapv str/trim (str/split in #"->"))]
    (mapv str->point points)))

(defn straight-line?
  "Either horizontal or vertical line?"
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn gen-points
  "Returns all discrete points on the line.
  Lines can be horizontal, vertical or precisely diagonal."
  [[start end]]
  (let [[x1 y1] start
        [x2 y2] end
        x-fn (cond
               (= x1 x2) identity
               (> x1 x2) dec
               :else inc)
        y-fn (cond
               (= y1 y2) identity
               (> y1 y2) dec
               :else inc)]
    (loop [x x1
           y y1
           points []]
      (if (and (= x x2) (= y y2))
        ;; last point reached
        (conj points [x y])
        ;; add a point and move!
        (recur (x-fn x) (y-fn y) (conj points [x y]))))))

(defn solve-1
  [file]
  (->> file
       util/read-input-file
       (mapv parse-line)
       (filter straight-line?)
       (mapcat gen-points)
       frequencies
       (keep (fn [[k v]] (when (>= v 2) k)))
       count))

(defn solve-2
  [file]
  (->> file
       util/read-input-file
       (mapv parse-line)
       (mapcat gen-points)
       frequencies
       (keep (fn [[k v]] (when (>= v 2) k)))
       count))
