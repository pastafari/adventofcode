(ns day3
  (:require [util :as u]))

(defn make-schematic
  "Takes lines, creates matrix"
  [lines]
  (into [] lines))

(defn re-matches-with-pos
  [re s]
  (loop [m (re-matcher re s)
         res []]
    (if (.find m)
      (recur m
             (conj res {:num (.group m)
                        :start (.start m)
                        ;; .end returns the offset after the match.
                        :end (dec (.end m))}))
      res)))

(defn find-numbers-with-pos
  "Finds numbers in the line with indices"
  [row-idx line]
  (let [ms (re-matches-with-pos #"(\d+)" line)]
    (map #(assoc % :row row-idx) ms)))

(defn find-*-with-pos
  [row-idx line]
  (let [ms (re-matches-with-pos #"\*" line)]
    (map #(assoc % :row row-idx) ms)))

(defn neighbors
  "Gets all neighbors given a row (r), start (i) and end (j) index,
  so we can find if there is an adjacent symbol. Returns a seq containing
  all indices considered adjacent. Relies on the fact that get-in returns nil
  even when index is out of bounds."
  [r i j]
  (let [gen-row (fn [row] (partition 2 (interleave (repeat row)
                                                  ;; range doesnt include end
                                                  (range (dec i)
                                                         (+ j 2)))))
        above (gen-row (dec r))
        below (gen-row (inc r))
        same-row [[r (dec i)] [r (inc j)]]]
    (concat above below same-row)))

(defn part-number?
  "true iff the given number is not adjacent to a symbol"
  [schematic {:keys [row start end]}]
  (let [ns (neighbors row start end)
        dot-or-num-re #"\.|\d"]
    ;; not every neighbor is a dot or a number
    ;; i.e. some neighbor is a symbol
    (not (every? (fn [[i j]]
                   (let [c (get-in schematic [i j])]
                     ;; c might be nil for edge cases,
                     ;; this counts as not next to symbol
                     (or (not c)
                         (re-find dot-or-num-re (str c)))))
                 ns))))

(defn gear?
  "true iff adjacent to exactly two numbers"
  [schematic {:keys [start row] :as _gear-symbol}]
  (let [adjacent-nums (->> schematic
                           (map-indexed find-numbers-with-pos)
                           (apply concat)
                           (filter (fn [{r :row s :start e :end}]
                                     (and (#{row (inc row) (dec row)} r)
                                          (<= (dec s) start (inc e))))))]
    (when (= 2 (count adjacent-nums))
      adjacent-nums)))

(defn solve
  [in]
  (let [schematic (->> in
                       u/input-seq
                       make-schematic)]
    (->> schematic
         (map-indexed find-numbers-with-pos)
         (apply concat)
         (filter #(part-number? schematic %))
         (map #(Integer/parseInt (:num %)))
         (apply +))))

(defn solve-2
  [in]
  (let [schematic (->> in
                       u/input-seq
                       make-schematic)]
    (->> schematic
         (map-indexed find-*-with-pos)
         (apply concat)
         (map (partial gear? schematic))
         (map #(if %
                 (let [x (Integer/parseInt (:num (first %)))
                       y (Integer/parseInt (:num (second %)))]
                   (* x y))
                 0))
         (apply +))))
