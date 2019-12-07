(ns advent-2019.amplification-circuit
  (:require [advent-2019.sunny-with-a-chance-of-asteroids :as intcode]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn eval-amplify
  "An amplifier has two inputs: phase and input.
  These are supplied here as arguments.
  When program is eval-ed as Intcode, it can ask for input:
  - the first time we supply the value of phase as input,
  - the second time we supply the value of input as input.
  When eval-program writes output,
  - we capture it in a string
  We return the last output as the value of this function."
  [program phase input]
  (let [input-string (str phase "\n" input)
        output-string (with-out-str
                        (with-in-str input-string
                          (intcode/eval-program program)))]
    (Integer/parseInt (str/trim output-string))))


(defn eval-amplifier-circuit
  "Evaluates an amplifier circuit with the given sequence of phases
  an Intcode program and a start-input to the first amplifier.
  The output of an amplifier is fed as input to the next amplifier"
  [phases program]
  (reduce (fn [input phase] (eval-amplify program phase input))
          0 ;; represents the input to the first amplifier, defined as 0.
          phases))


(defn find-max-thruster-signal
  "evals program for all possible DISTINCT phase combinations,
  From instruction: Each phase setting is used exactly once
  finds max output"
  [program phase-range]
  (let [thruster-signals (for [a phase-range
                               b phase-range
                               c phase-range
                               d phase-range
                               e phase-range
                               :when (= (distinct [a b c d e]) [a b c d e])]
                           (eval-amplifier-circuit [a b c d e] program))]
    (apply max thruster-signals)))

(defn solve-1
  [file]
  (let [program (intcode/read-program file)]
    (find-max-thruster-signal program (range 5))))

(deftest test-eval-amplifier-circuit
  (is (= 43210 (eval-amplifier-circuit [4 3 2 1 0]
                                       [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))

  (is (= 54321 (eval-amplifier-circuit [0 1 2 3 4]
                                       [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0])))
  (is (= 65210 (eval-amplifier-circuit [1 0 4 3 2]
                                       [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])))
  )


(deftest test-find-max-thruster-signal
  (is (= 43210 (find-max-thruster-signal [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))
  (is (= 54321 (find-max-thruster-signal [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0])))
  (is (= 65210 (find-max-thruster-signal [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0]))))
