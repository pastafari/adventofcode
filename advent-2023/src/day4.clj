(ns day4
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [util :as u]))

(defn make-card
  [line]
  (let [[game nums] (str/split line #":")
        [w o] (str/split nums #"\|")
        str->nums (fn [s] (map #(Integer/parseInt %)
                              (re-seq #"\d+" s)))]
    {:game (->> game (re-find #"\d+") Integer/parseInt)
     :wins (str->nums w)
     :ours (str->nums o)}))

(defn win-count [{:keys [wins ours]}]
  (count (set/intersection (set wins) (set ours))))

(defn get-points
  "Verbose power of 2 thing!"
  [card]
  (let [w-count (win-count card)]
    (if (zero? w-count)
          0
          (apply * 1 (repeat (dec w-count) 2)))))

(defn make-copies-fast
  "Uses the insight that card number n can only be introduced by cards < n."
  [cards]
  (let [;; **assume** that cards are sorted by game number
        ;; initially only 1 of each card
        card-counts (zipmap (map :game cards) (repeat 1))]
    (reduce (fn [totals {:keys [game] :as card}]
              (let [w-count (win-count card)
                    new-cards (if (pos? w-count)
                                (vec (range (inc game)
                                            (+ 1 w-count game)))
                                [])
                    ;; how many copies of current card do we have?
                    num-copies (get totals game)
                    added-cards (zipmap new-cards (repeat num-copies))]
                (merge-with + totals added-cards)))
            card-counts
            cards)))

(defn solve
  [in]
  (->> in
       u/input-seq
       (map make-card)
       (map get-points)
       (apply +)))

(defn solve-2
  [in]
  (->> in
       u/input-seq
       (map make-card)
       make-copies-fast))
