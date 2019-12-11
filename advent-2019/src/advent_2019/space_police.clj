(ns advent-2019.space-police
  (:require [advent-2019.sunny-with-a-chance-of-asteroids :as intcode]
            [advent-2019.amplification-circuit :as circuit]
            [advent-2019.protocols.amplifiers :as amplifiers]))

(def BLACK 0)

(def WHITE 1)

(def LEFT 0)

(def RIGHT 1)

(defn get-orientation
  "There's probably a nicer way to do this!"
  [orientation direction]
  (case direction
    0 (case orientation
        :north :west
        :south :east
        :west :south
        :east :north)
    1 (case orientation
        :north :east
        :south :west
        :west :north
        :east :south)))

(defn get-new-position
  "Walk one step towards orientation"
  [[x y] orientation]
  (case orientation
    :north [x (inc y)]
    :south [x (dec y)]
    :east [(inc x) y]
    :west [(dec x) y])) 

(defn move
  [{:keys [position orientation]} direction]
  (let [new-orientation (get-orientation orientation direction)
        new-position (get-new-position position new-orientation)]
    {:position new-position
     :orientation new-orientation}))


(defn paint-hull
  "The robot needs to be able to move around on the grid of square panels on the side of your ship, detect the color of its current panel, and paint its current panel black or white. **All of the panels are currently black.**
  The program uses input instructions to access the robot's camera: provide 0 if the robot is over a black panel or 1 if the robot is over a white panel. Then, the program will output two values:

    First, it will output a value indicating the color to paint the panel the robot is over: 0 means to paint the panel black, and 1 means to paint the panel white.
    Second, it will output a value indicating the direction the robot should turn: 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees.

  After the robot turns, it should always move forward exactly one panel. The robot starts facing up.
  
  "
  [program]
  (let [input (circuit/build-wire "in")
        output (circuit/build-wire "out")
        ;; hack to use the phase input as the first color!
        evaluator (circuit/build-amplifier program BLACK input output)
        robot-state (atom {:orientation :north
                           :position [0 0]
                           :white-panels #{}
                           :painted-panels #{}})]
    ;; start evaluating the program, first input is already provided by phase, so BLACK!
    (amplifiers/start evaluator)
    (while (not (amplifiers/halted? evaluator))
      (let [paint-color (amplifiers/read-instruction output)
            turn-direction (amplifiers/read-instruction output)
            curr-position (:position @robot-state)
            curr-orientation (:orientation @robot-state)
            {:keys [position orientation]} (move @robot-state turn-direction)
            next-color (if ((@robot-state :white-panels) position)
                         WHITE
                         BLACK)]
        (swap! robot-state update-in [:painted-panels] conj curr-position)
        (if (= paint-color WHITE)
          (swap! robot-state update-in [:white-panels] conj curr-position)
          (swap! robot-state update-in [:white-panels] disj curr-position))
        ;; move to new position and provide its color as input
        (swap! robot-state assoc :position position)
        (swap! robot-state assoc :orientation orientation)
        (amplifiers/write input next-color)
        (println "position: " position " orientation: " orientation " paint-color: " paint-color)))
    robot-state))
