(ns advent-2020.day5
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]))


(defn make-boarding-pass
  "Takes a line of the form FBFBBFFRLR and makes a boarding pass from it.
  A boarding pass is a map containing :row, :column, :seat-id"
  [line]
  (let [binary-str (-> line
                       (str/replace #"F|L" "0")
                       (str/replace #"B|R" "1"))
        row (Integer/parseInt (apply str (take 7 binary-str)) 2)
        column (Integer/parseInt (apply str (drop 7 binary-str)) 2)
        seat-id (+ column (* 8 row))]
    {:row row
     :column column
     :seat-id seat-id}))


(defn day5-1
  "Reads file, finds highest seat id"
  [file]
  (->> file
      utils/read-input-file
      (map make-boarding-pass)
      (map :seat-id)
      (apply max)))

(defn day5-2
  "Reads file, finds missing seat. Such hack much wow!"
  [file]
  (->> file
       utils/read-input-file
       (map make-boarding-pass)
       (sort-by :seat-id)
       (map :seat-id)
       (println)))

(deftest make-boarding-pass-test
  (testing "it makes the right boarding passes!"
    (is (= {:row 70 :column 7 :seat-id 567} (make-boarding-pass "BFFFBBFRRR")))
    (is (= {:row 14 :column 7 :seat-id 119} (make-boarding-pass "FFFBBBFRRR")))
    (is (= {:row 102 :column 4 :seat-id 820} (make-boarding-pass "BBFFBBFRLL")))))
