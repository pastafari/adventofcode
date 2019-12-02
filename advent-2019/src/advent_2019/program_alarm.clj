(ns advent-2019.program-alarm
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn halt?
  [op]
  (= op 99))


(defn eval-add
  "Evaluates an add operation and returns program"
  [program position]
  (let [index1 (program (+ position 1))
        index2 (program (+ position 2))
        destination (program (+ position 3))]
    (assoc-in program [destination] (+ (program index1)
                                       (program index2)))))

(defn eval-multiply
  "Evaluates a multiple operation and returns program"
  [program position]
  (let [index1 (program (+ position 1))
        index2 (program (+ position 2))
        destination (program (+ position 3))]
    (assoc-in program [destination] (* (program index1)
                                       (program index2)))))

(defn eval-op
  "Evals the op at the given position and returns new program"
  [program position]
  (let [op (program position)]
    (cond
      (= op 1) (eval-add program position)
      (= op 2) (eval-multiply program position))))

(defn eval-program
  "Evaluates a vector representing an Intcode program"
  [ops]
  (loop [program ops
         position 0]
    (let [op (program position)]
      (if (halt? op)
        program
        (recur (eval-op program position)
               (+ position 4))))))

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


(deftest eval-program-tests
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
