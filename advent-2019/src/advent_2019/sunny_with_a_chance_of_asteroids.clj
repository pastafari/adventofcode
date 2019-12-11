(ns advent-2019.sunny-with-a-chance-of-asteroids
  (:require [advent-2019.protocols.amplifiers :as amplifiers]   
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

;; This allows us to use *in* and *out* as defaults for reading
;; and writing. But makes the system extensible for Day 7 - Part 2.
(extend-protocol amplifiers/Input
  clojure.lang.LineNumberingPushbackReader
  (read-instruction [this] (Integer/parseInt (.readLine this))))

(extend-protocol amplifiers/Output
  java.io.PrintWriter
  (write [this v] (.println this v)))

(defn halt?
  [op]
  (= op 99))

(defn position-mode?
  [mode]
  (= 0 mode))

(defn immediate-mode?
  [mode]
  (= 1 mode))

(defn relative-mode?
  [mode]
  (= 2 mode))

(defn get-destination
  "Special case for op-codes that write to a destination.
  IMO this breaks the semantics of position and relative mode"
  [{:keys [relative-base] :as program} position mode]
  (if (relative-mode? mode)
    (+ relative-base (program position 0))
    (program position 0)))

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
    ;; we could be addressing memory with nothing in it,
    ;; which defaults to 0
    (position-mode? mode) (or (program (program position)) 0)
    (immediate-mode? mode) (program position)
    ;; relative is like position but relative to relative-base!
    (relative-mode? mode) (or (program (+ (:relative-base program)
                                          (program position)))
                              0)))

(defn eval-input
  "Waits for an input from an Input, then updates and returns program"
  [program position input {:keys [mode-1]}]
  (let [arg (amplifiers/read-instruction input)
        destination (get-destination program (inc position) mode-1)]
    {:program (assoc-in program [destination] arg)
     :position (+ position 2)}))

(defn eval-output
  "Writes a val to an Output. Returns program"
  [program position output {:keys [mode-1]}]
  (let [arg (get-argument program (inc position) mode-1)]
    (amplifiers/write output arg)
    {:program program
     :position (+ position 2)}))
 
(defn eval-add
  "Evaluates an add operation and returns program"
  [program position {:keys [mode-1 mode-2 mode-3]}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (get-destination program (+ position 3) mode-3)]
    {:program (assoc-in program [destination] (+ arg1
                                                 arg2))
     :position (+ position 4)}))

(defn eval-multiply
  "Evaluates a multiple operation and returns program"
  [program position {:keys [mode-1 mode-2 mode-3] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (get-destination program (+ position 3) mode-3)]
    {:program (assoc-in program [destination] (* arg1
                                                 arg2))
     :position (+ position 4)}))

(defn eval-jump-if-true
  "if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing."
  [program position {:keys [op mode-1 mode-2] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        new-position (if (not (zero? arg1))
                       arg2
                       (+ position 3))]
    {:program program
     :position new-position}))

(defn eval-jump-if-false
  "if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing."
  [program position {:keys [op mode-1 mode-2] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        new-position (if (zero? arg1)
                       arg2
                       (+ position 3))]
    {:program program
     :position new-position}))

(defn eval-less-than
  "if the first parameter is less than the second parameter,
  it stores 1 in the position given by the third parameter.
  Otherwise, it stores 0."
  [program position {:keys [op mode-1 mode-2 mode-3] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (get-destination program (+ position 3) mode-3)
        new-position (+ position 4)
        value (if (< arg1 arg2) 1 0)]
    {:program (assoc-in program [destination] value)
     :position new-position}))

(defn eval-equals
  "if the first parameter is equal to the second parameter,
  it stores 1 in the position given by the third parameter.
  Otherwise, it stores 0."
  [program position {:keys [op mode-1 mode-2 mode-3] :as parsed-op}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        arg2 (get-argument program (+ position 2) mode-2)
        destination (get-destination program (+ position 3) mode-3)
        new-position (+ position 4)
        value (if (= arg1 arg2) 1 0)]
    {:program (assoc-in program [destination] value)
     :position new-position}))

(defn eval-relative-base-offset
  "adjusts the relative base by the value of its only parameter.
  The relative base increases (or decreases, if the value is negative)
  by the value of the parameter"
  [program position {:keys [mode-1]}]
  (let [arg1 (get-argument program (+ position 1) mode-1)
        new-position (+ position 2)]
    {:program (update-in program [:relative-base] #(+ arg1 %))
     :position new-position}))

(defn eval-op
  "Evals the op at the given position and returns new program and position,
  optionally returns a :halted? flag if done."
  [{:keys [relative-base] :as program-state} position input output]
  (let [{:keys [op] :as parsed-op} (parse-op (program-state position))]
    (cond
      (= op 1) (eval-add program-state position parsed-op)
      (= op 2) (eval-multiply program-state position parsed-op)
      (= op 3) (eval-input program-state position input parsed-op)
      (= op 4) (eval-output program-state position output parsed-op)
      (= op 5) (eval-jump-if-true program-state position parsed-op)
      (= op 6) (eval-jump-if-false program-state position parsed-op)
      (= op 7) (eval-less-than program-state position parsed-op)
      (= op 8) (eval-equals program-state position parsed-op)
      (= op 9) (eval-relative-base-offset program-state position parsed-op)
      (halt? op) {:program program-state :position position :halted? true})))


(defn eval-program
  "Evaluates a vector representing an Intcode program.
  Creates an internal representation as a map to manage relative-base and
  allow for arbitrary memory addressing.
  Allows input-stream and output-stream to be configured via optional keys"
  [ops {:keys [input output] :or {input *in* output *out*}}]
  (let [initial-program (into {:relative-base 0} (map-indexed vector ops))]
    (loop [program initial-program
           position 0]
      (let [{:keys [program position halted?] :or {halted? false} :as result}
            (eval-op program position input output)]
        (if halted?
          result
          (recur program
                 position))))))

(defn read-program
  "Reads in csv input from file and returns a vector of integers
  representing the Intcode program"
  [file]
  (vec (map #(Long/parseLong %)
            (str/split
             (str/trim (slurp (io/reader (io/resource file))))
             #","))))

(defn solve
  "Reads program and evals it"
  [file]
  (eval-program (read-program file)))


(deftest test-eval-program
  (is (= {:relative-base 0, 0 3500, 7 0, 1 9,  4 2, 6 11, 3 70, 2 10, 11 50, 9 30, 5 3, 10 40, 8 99}
         (:program (eval-program [1 9 10 3 2 3 11 0 99 30 40 50] {}))))
  (is (= {:relative-base 0, 0 2, 1 0, 2 0, 3 0, 4 99}
         (:program (eval-program [1 0 0 0 99] {}))))
  (is (= {:relative-base 0, 0 2, 1 3, 2 0, 3 6, 4 99}
         (:program (eval-program [2 3 0 3 99] {}))))
  (is (= {:relative-base 0, 0 2, 1 4, 2 4, 3 5, 4 99, 5 9801}
         (:program (eval-program [2 4 4 5 99 0] {}))))
  (is (= {0 30, 7 0, 1 1, :relative-base 0, 4 2, 6 6, 3 4, 2 1, 5 5, 8 99}
         (:program (eval-program [1 1 1 4 99 5 6 0 99] {})))))


(deftest test-parse-op
  (is (= {:op 2 :mode-1 0 :mode-2 1 :mode-3 0} (parse-op 1002)))
  (is (= {:op 2 :mode-1 0 :mode-2 0 :mode-3 0} (parse-op 2)))
  (is (= {:op 4 :mode-1 1 :mode-2 0 :mode-3 0} (parse-op 104))))


(deftest test-get-argument
  (is (= 222 (get-argument {0 222 1 0 2 0 3 0} 1 0)))
  (is (= 0 (get-argument {0 222 1 0 2 0 3 0} 1 1))))
