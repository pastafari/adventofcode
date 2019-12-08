(ns advent-2019.amplification-circuit
  (:require [advent-2019.sunny-with-a-chance-of-asteroids :as intcode]
            [advent-2019.protocols.amplifiers :as amplifiers]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import (java.util.concurrent ArrayBlockingQueue)))

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (println "Uncaught exception on" (.getName thread))
     (.printStackTrace ex))))

(defrecord Wire [name queue]
  amplifiers/Input
  (read-instruction [this]
    (let [next (.take queue)]
      (println name " received: " next)
      next))
  amplifiers/Output
  (write [this v]
    (.put queue v)))

(defn build-wire
  [name]
  (let [queue (ArrayBlockingQueue. 1000)]
    (Wire. name queue)))

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

(defn build-amplifier
  "Builds a stateful amplifier which remembers its program memory
  and instruction pointer during evaluation"
  [program phase input output]
  (let [program-state (atom {:program program
                             :position 0
                             :halted? false})]
    (reify amplifiers/Amplifier
      (start [this] (do
                      (amplifiers/write input phase) ;; the first input is always phase.
                      (.start
                       (Thread.
                        (fn []
                          (while (not (amplifiers/halted? this))
                            (let [new-state (intcode/eval-op (:program @program-state)
                                                             (:position @program-state)
                                                             input
                                                             output)]
                              (swap! program-state (constantly new-state)))))))))
      (halted? [this] (:halted? @program-state))
      (inspect [this] {:position (:position @program-state)
                       :phase phase
                       :halted? (:halted? @program-state)
                       :input input
                       :output output}))))

(defn phase-combinations
  "Generates all possible phase combinations for 5 amplifiers"
  [start end]
  (let [phase-range (range start (inc end))]
    (for [a phase-range
          b phase-range
          c phase-range
          d phase-range
          e phase-range
          :when (= (distinct [a b c d e]) [a b c d e])]
      [a b c d e])))

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

(defn build-amplifier-circuit
  "Builds a new amplifier circuit with new wires!"
  [program phases]
  (let [a-b (build-wire "a-b")
        b-c (build-wire "b-c")
        c-d (build-wire "c-d")
        d-e (build-wire "d-e")
        e-a (build-wire "e-a")
        [pa pb pc pd pe] phases]
    {:amplifiers [(build-amplifier program pa e-a a-b)
                  (build-amplifier program pb a-b b-c)
                  (build-amplifier program pc b-c c-d)
                  (build-amplifier program pd c-d d-e)
                  (build-amplifier program pe d-e e-a)]
     :wires [a-b b-c c-d d-e e-a]}))


(defn eval-circuit-with-feedback
  "Evaluates the result of running a circuit.
  - Start the circuit.
  - Send 0 to amp A exactly once, thus we write the instruction 0
  to wire e-a.
  - Return the circuit as result"
  [{:keys [amplifiers wires] :as circuit}]
  (let [wire-e-a (last wires)]
    (doseq [a amplifiers]
      (amplifiers/start a)) ;; start circuit
    (amplifiers/write wire-e-a 0)  ;; write 0 to wire e-a
    ;; wait for circuit to halt
    (while (not (every? amplifiers/halted? amplifiers))
      (Thread/sleep 10))
    ;; result is output on the last wire.
    (amplifiers/read-instruction wire-e-a)))

(defn solve-1
  [file]
  (let [program (intcode/read-program file)]
    (find-max-thruster-signal program (range 5))))

(defn solve-2
  [file]
  (let [program (intcode/read-program file)
        phase-ranges (phase-combinations 5 9)
        circuit-outputs (doall
                            (for [phases phase-ranges]
                              (let [circuit (build-amplifier-circuit program phases)]
                                (future (eval-circuit-with-feedback circuit)))))]
    (apply max (map deref circuit-outputs))))

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
