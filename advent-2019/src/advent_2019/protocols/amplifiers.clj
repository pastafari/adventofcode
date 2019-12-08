(ns advent-2019.protocols.amplifiers)

(defprotocol Input
  "Represents an Input to an Intcode instruction"
  (read-instruction [this] "Reads an input string, can block"))

(defprotocol Output
  (write [this v] "Writes an output string, can block"))

(defprotocol Amplifier
  (start [this])
  (halted? [this])
  (inspect [this]))

