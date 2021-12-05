(ns advent-2021.day4
  (:require [advent-2021.util :as util]
            [clojure.string :as str]))

;; just for documentation!
(def cell-states #{:marked :unmarked})

(defn make-moves
  [ms]
  (->> (str/split ms #",")
       (mapv #(Integer/parseInt %))))

(defn make-row
  "Makes the nth row on a board."
  [row-num row]
  (->> (str/split (str/trim row) #"\s+")
       (map-indexed (fn [idx n] [(Integer/parseInt n) row-num idx]))))

(defn make-board
  "Takes a seq of 5 strings and returns an unmarked board"
  [rows]
  (apply merge
   (map-indexed (fn [idx row]
                  (let [row* (make-row idx row)]
                    (into {}
                          (map (fn [[num i j]]
                                 [num {:pos [i j]
                                       :state :unmarked}])
                               row*))))
                rows)))

(defn make-moves-and-boards
  [inputs]
  (let [moves (first inputs)
        boards (rest inputs)]
    {:moves (make-moves (first moves))
     :boards (mapv make-board boards)}))

(defn mark-number
  "Returns boards with number marked."
  [number boards]
  (map (fn [board]
         (if (board number)
           (update board number assoc :state :marked)
           board))
       boards))

(defn filter-cells-by-state
  [board s]
  (filter (fn [[_ {:keys [state]}]]
            (= s state))
          board))

(defn marked-cells
  [board]
  (filter-cells-by-state board :marked))

(defn unmarked-cells
  [board]
  (filter-cells-by-state board :unmarked))

(defn five-of-the-same?
  [xs n]
  (= 5 (count (filter #{n} xs))))

(defn column-bingo?
  [cells]
  (let [cols (->> cells
                  (mapv (comp second :pos second)))]
    (some (partial five-of-the-same? cols) (range 5))))

(defn row-bingo?
  [cells]
  (let [rows (->> cells
                  (mapv (comp first :pos second)))]
    (some (partial five-of-the-same? rows) (range 5))))

(defn bingo?
  [board]
  (let [marked (marked-cells board)]
    (and (>= (count marked) 5)
         (or (column-bingo? marked)
             (row-bingo? marked)))))

(defn find-winning-board
  "Finds first board that wins with given moves"
  [{:keys [moves boards]}]
  (loop [bs boards
         ms moves]
    (let [num (first ms)
          marked-boards (mark-number num bs)]
      (if-let [winner (some #(when (bingo? %) %)
                            marked-boards)]
        {:board winner :number num}
        (if (seq (rest ms))
          (recur marked-boards (rest ms))
          {:board nil :number nil})))))

(defn play-all-numbers
  [{:keys [moves boards]}]
  (loop [winners #{}
         round 0
         bs boards
         ms moves]
    (if (and (seq ms) (seq bs))
      (let [num (first ms)
            marked-boards (mark-number num bs)]
        (if-let [round-winners (filter #(when (bingo? %) %)
                                       marked-boards)]
          (recur (apply conj winners (mapv (fn [w] {:winner w
                                                   :number num
                                                   :round round})
                                           round-winners))
                 (inc round)
                 (remove #((set round-winners) %) marked-boards)
                 (rest ms))
          (recur winners
                 (inc round)
                 marked-boards
                 (rest ms))))
      winners)))

(defn read-input
  "Returns a map with
  :moves - a seq of numbers to be marked
  :boards - a seq of original unmarked boards."
  [file]
  (->> file
       util/read-input-file
       (partition-by #(= "" %))
       (remove #(= '("") %))
       make-moves-and-boards))

(defn solve-1
  [file]
  (let [{:keys [board number]} (-> file
                                   read-input
                                   find-winning-board)
        unmarked (unmarked-cells board)
        sum (apply + (map first unmarked))]
    (* sum number)))

(defn solve-2
  [file]
  (let [winners (-> file
                    read-input
                    play-all-numbers)
        {:keys [winner number]} (last (sort-by :round winners))
        unmarked (unmarked-cells winner)
        sum (apply + (map first unmarked))]
    (* sum number)))
