(ns day5
  (:require
   [clojure.string :as str]
   [util :as u]))

(defn lines-between
  "Returns a fn that takes a seq and returns things
  between str x and str y (both not inclusive)"
  [in x y]
  (->> in
       (drop-while #(not= x %))
       (drop 1) ;; drops x
       (take-while #(not= y %)) ;; drops until y
       (drop-last) ;; drops empty line
       ))

(defn line->range
  [line]
  (let [[d s r] (re-seq #"\d+" line)]
    {:d (biginteger d)
     :s (biginteger s)
     :r (biginteger r)}))

(defn make-ranged-fn
  [ranges]
  (memoize
   (fn [input]
     (or (some (fn [rng]
                 (let [{:keys [s d r]} rng]
                   (when (<= s input (+ s r -1))
                     (+ d (- input s)))))
               ranges)
         input))))

(defn parse-between
  [in-seq x y]
  (let [lines (lines-between in-seq x y)]
    (map line->range lines)))

(defn seed-to-soil
  [in]
  (-> in
      (parse-between "seed-to-soil map:" "soil-to-fertilizer map:")
      make-ranged-fn))

(defn soil-to-fertilizer
  [in]
  (-> in
      (parse-between "soil-to-fertilizer map:" "fertilizer-to-water map:")
      make-ranged-fn))

(defn fertilizer-to-water
  [in]
  (-> in
      (parse-between "fertilizer-to-water map:" "water-to-light map:")
      make-ranged-fn))

(defn water-to-light
  [in]
  (-> in
      (parse-between "water-to-light map:" "light-to-temperature map:")
      make-ranged-fn))

(defn light-to-temperature
  [in]
  (-> in
      (parse-between "light-to-temperature map:" "temperature-to-humidity map:")
      make-ranged-fn))

(defn temperature-to-humidity
  [in]
  (-> in
      (parse-between "temperature-to-humidity map:" "humidity-to-location map:")
      make-ranged-fn))

(defn humidity-to-location
  "Special case! Last thing."
  [in]
  (->> in
       (drop-while #(not= "humidity-to-location map:" %))
       (drop 1)
       (map line->range)
       make-ranged-fn))

(defn make-maps
  [in]
  {:seed-to-soil (seed-to-soil in)
   :soil-to-fertilizer (soil-to-fertilizer in)
   :fertilizer-to-water (fertilizer-to-water in)
   :water-to-light (water-to-light in)
   :light-to-temperature (light-to-temperature in)
   :temperature-to-humidity (temperature-to-humidity in)
   :humidity-to-location (humidity-to-location in)})

(defn make-location-fn
  [{:keys [seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature temperature-to-humidity humidity-to-location]}]
  (comp humidity-to-location
        temperature-to-humidity
        light-to-temperature
        water-to-light
        fertilizer-to-water
        soil-to-fertilizer
        seed-to-soil))

(defn parse-seeds
  [in]
  (let [num-str (-> in
                    u/input-seq
                    first
                    (str/split #":")
                    second
                    str/trim)]
    (map #(biginteger %)
         (re-seq #"\d+" num-str))))

(defn parse-seeds-2
  [in]
  (let [num-str (-> in
                    u/input-seq
                    first
                    (str/split #":")
                    second
                    str/trim)]
    (->> num-str
         (re-seq #"\d+")
         (map #(biginteger %))
         (partition 2)
         (mapcat (fn [[s e]] (range s (+ s e)))))))

(defn parse-input
  "Returns a map of whats in the file"
  [in]
  (->> in
       u/input-seq
       make-maps
       make-location-fn))

(defn solve
  [in]
  (let [seeds (parse-seeds in)
        loc-fn (parse-input in)]
    (apply min (map loc-fn seeds))))

(defn solve-2
  [in]
  (let [seeds (parse-seeds-2 in)
        loc-fn (parse-input in)]
    (apply min (pmap loc-fn seeds))))
