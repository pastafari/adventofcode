(ns advent-2019.universal-orbit-map
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;; The center of mass of the system, a special name!
(def COM "COM")

(defn build-orbit-graph
  "Takes a vector of pairs [a b] where b orbits a
  and builds a tree with a single root"
  [orbits]
  (apply uber/graph orbits))

(defn count-direct-and-indirect-orbits
  "Counts edges from each node to COM - the special node
  representing the center of mass"
  [orbits]
  (let [graph (build-orbit-graph orbits)]
    (reduce (fn [acc node] (+ acc
                             (:cost (alg/shortest-path graph node COM))))
            0
            (uber/nodes graph))))

(defn read-orbits
  "Reads in orbits from input file as [a b] implying b orbits a"
  [file]
  (let [orbits (line-seq (io/reader (io/resource file)))]
    (map #(str/split % #"\)") orbits)
    ))

(defn solve-1
  [file]
  (-> file
      read-orbits
      count-direct-and-indirect-orbits))
