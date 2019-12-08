(ns advent-2019.space-image-format
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn encoded-image->layers
  "Converts an encoded image to layers given width and height"
  [image width height]
  (let [layer-dimension (* width height)]
    (partition layer-dimension image)))


(defn read-encoded-image
  "Reads image at given location as vector of ints"
  [file]
  (let [image-string (-> file
                         io/resource
                         io/file
                         slurp
                         str/trim)]
    (map #(Integer/parseInt (str %)) image-string)))


(defn count-digits
  "Counts occurences of digit in layer"
  [layer digit]
  (count (filter #(= % digit) layer)))

(defn find-layer-with-fewest-occurrences
  "Finds the layer with the fewest occurences of digit"
  [layers digit]
  (:layer
   (reduce (fn [{:keys [min-zeros layer] :as min-so-far} l]
             (let [zeros (count-digits l 0)]
               (if (< zeros min-zeros)
                 {:min-zeros zeros
                  :layer l}
                 min-so-far)))
           {:min-zeros Integer/MAX_VALUE}
           layers)))

(defn solve-1
  [file width height]
  (let [image (read-encoded-image file)
        layers (encoded-image->layers image width height)
        layer-with-min-zeros (find-layer-with-fewest-occurrences layers 0)]
    (* (count-digits layer-with-min-zeros 1)
       (count-digits layer-with-min-zeros 2))))


(deftest test-count-digits
  (is (= 4 (count-digits [0 1 2 3 0 1 2 3 0 0] 0))))

(deftest test-encoded-image->layers
  (is (= [[1 2 3 4 5 6] [7 8 9 0 1 2]]
         (encoded-image->layers [1 2 3 4 5 6 7 8 9 0 1 2] 3 2))))
