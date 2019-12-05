(ns advent-2019.sunny-with-a-chance-of-asteroids
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn halt?
  [op]
  (= op 99))

(defn position-mode?
  [mode]
  (= 0 mode))

(defn immediate-mode?
  [mode]
  (= 1 mode))

(defn parse-op
  "Parses an op that of the form ABCDE. Pads ops with leading zeros to fit format.
  See test-parse-op for details"
  [op]
  (let [[a b c d e] (format "%05d" op)
        opcode (Integer/parseInt (apply str [d e]))
        mode-1 (Integer/parseInt (apply str [c]))
        mode-2 (if b
                 (Integer/parseInt (apply str [b]))
                 0)
        mode-3 (if a
                 (Integer/parseInt (apply str [a]))
                 0)]
    {:op opcode :mode-1 mode-1 :mode-2 mode-2 :mode-3 mode-3}))

(defn get-argument
  "Gets argument depending on immediate v/s position mode"
  [program position mode]
  (cond
    (position-mode? mode) (program (program position))
    (immediate-mode? mode) (program position)))

(defn eval-input
  "Waits for an input, then processes it and returns program"
  [program position _]
  (let [arg (Integer/parseInt (read-line))
        destination (program (inc position))]
    {:program (assoc-in program [destination] arg)
     :jump 2}))

(defn eval-output
  "Outputs args to stdout. Returns program"
  [program position {:keys [mode-1]}]
  (let [arg (get-argument program (inc position) mode-1)]
    (println arg)
    {:program program
     :jump 2}))
 
(defn eval-add
  "Evaluates an add operation and returns program"
  [program position {:keys [mode-1 mode-2 mode-3]}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (program (+ position 3))]
    {:program (assoc-in program [destination] (+ arg1
                                                 arg2))
     :jump 4}))

(defn eval-multiply
  "Evaluates a multiple operation and returns program"
  [program position {:keys [mode-1 mode-2 mode-3] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (program (+ position 3))]
    {:program (assoc-in program [destination] (* arg1
                                                 arg2))
     :jump 4}))

(defn eval-op
  "Evals the op at the given position and returns new program"
  [program position {:keys [op] :as parsed-op}]
  (cond
    (= op 1) (eval-add program position parsed-op)
    (= op 2) (eval-multiply program position parsed-op)
    (= op 3) (eval-input program position parsed-op)
    (= op 4) (eval-output program position parsed-op)
    ;;(= op 5) (eval-jump-if-true program position parsed-op)
    ;;(= op 6) (eval-jump-if-false program position parsed-op)
    ;;(= op 7) (eval-less-than program position parsed-op)
    ;;(= op 8) (eval-equals program position parsed-op)
    ))


(defn eval-program
  "Evaluates a vector representing an Intcode program"
  [ops]
  (loop [program ops
         position 0]
    (let [{:keys [op] :as parsed-op} (parse-op (program position))
          {:keys [program jump]} (eval-op program position parsed-op)]      
      (if (halt? op)
        program
        (recur program
               (+ position jump))))))

(defn read-program
  "Reads in csv input from file and returns a vector of integers
  representing the Intcode program"
  [file]
  (vec (map #(Integer/parseInt %)
            (str/split
             (str/trim (slurp (io/reader (io/resource file))))
             #","))))

(defn solve-1
  "Reads program and evals it"
  [file]
  (eval-program (read-program file)))


(deftest test-eval-program
  (is (= [3500 9 10 70 2 3 11 0 99 30 40 50]
         (eval-program [1 9 10 3 2 3 11 0 99 30 40 50])))
  (is (= [2 0 0 0 99]
         (eval-program [1 0 0 0 99])))
  (is (= [2 3 0 6 99]
         (eval-program [2 3 0 3 99])))
  (is (= [2 4 4 5 99 9801]
         (eval-program [2 4 4 5 99 0])))
  (is (= [30 1 1 4 2 5 6 0 99]
         (eval-program [1 1 1 4 99 5 6 0 99]))))


(deftest test-parse-op
  (is (= {:op 2 :mode-1 0 :mode-2 1 :mode-3 0} (parse-op 1002)))
  (is (= {:op 2 :mode-1 0 :mode-2 0 :mode-3 0} (parse-op 2)))
  (is (= {:op 4 :mode-1 1 :mode-2 0 :mode-3 0} (parse-op 104))))


(deftest test-get-argument
  (is (= 222 (get-argument [222 0 0 0] 1 0)))
  (is (= 0 (get-argument [222 0 0 0] 1 1))))
