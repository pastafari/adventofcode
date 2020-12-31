(ns advent-2020.day8
  (:require [advent-2020.utils :as utils]
            [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(defn parse-line
  "Takes a line of the form op signed_int and parses it into an opcode and a count"
  [line]
  (let [[opcode count-str] (str/split line #" ")]
    [opcode (Integer/parseInt count-str)]))

(defn parse-program
  "Parses the input line-seq into a program represented as a vector of vectors"
  [lines]
  (mapv parse-line lines))

(defn eval-instruction
  "Evals instruction, returns next-acc and next-offset representing new values
  for accumulator and offset."
  [{:keys [program acc offset]}]
  (let [[op count] (nth program offset)]
    (case op
      "nop" {:next-acc acc
             :next-offset (inc offset)}
      "jmp" {:next-acc acc
             :next-offset (+ offset count)}
      "acc" {:next-acc (+ acc count)
             :next-offset (inc offset)})))


(defn eval-until-infinite-loop
  "Evals a program until it finds an instruction that has already been evaled.
  Returns value of accumulator before evaling the instruction a second time."
  [program]
  (loop [offset 0
         acc 0
         executed? #{}]
    (if (executed? offset)
      acc
      (let [next-instr (nth program offset)
            {:keys [next-acc next-offset]} (eval-instruction {:program program
                                                    :acc acc
                                                    :offset offset})]
        (recur next-offset next-acc (conj executed? offset))))))


(defn day8-1
  "Run your copy of the boot code.
Immediately before any instruction is executed a second time,
what value is in the accumulator?"
  [file]
  (-> file
      utils/read-input-file
      parse-program
      eval-until-infinite-loop))


(deftest test-eval-until-infinite-loop
  (testing "with test program it returns 5"
    (is (= 5 (eval-until-infinite-loop [["nop" 0]
                                        ["acc" 1]
                                        ["jmp" 4]
                                        ["acc" 3]
                                        ["jmp" -3]
                                        ["acc" -99]
                                        ["acc" 1]
                                        ["jmp" -4]
                                        ["acc" +6]])))))
