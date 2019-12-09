(ns advent-2019.space-image-format
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import (java.awt.image BufferedImage)))

(def BLACK 0)

(def WHITE 1)

(def TRANSPARENT 2)

(defn get-color
  "Finds the first non transparent color for the layer."
  [layer]
  (first (remove #{TRANSPARENT} layer)))

(defn layers->colors
  "Transposes layers and gets each pixel color"
  [layers width]
  (let [pixels (apply map vector layers)]
    (map get-color pixels)))


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

(defn image->colors
  [image width height]
  (let [layers (encoded-image->layers image width height)]
    (partition width (layers->colors layers width))))

(defn solve-1
  [file width height]
  (let [image (read-encoded-image file)
        layers (encoded-image->layers image width height)
        layer-with-min-zeros (find-layer-with-fewest-occurrences layers 0)]
    (* (count-digits layer-with-min-zeros 1)
       (count-digits layer-with-min-zeros 2))))

(defn solve-2
  [file width height]
  (image->colors (read-encoded-image file) width height))

(defn pixels->bitmap-image
  [pixels width height]
  (let [image (BufferedImage. 25 6 BufferedImage/TYPE_INT_RGB)]
    (for [x (range 25)
          y (range 6)
          :let [pixel (nth (nth solution-2 y) x)]]
      (.setRGB image
               x
               y
               (rand-int 255)))
    image))

(deftest test-count-digits
  (is (= 4 (count-digits [0 1 2 3 0 1 2 3 0 0] 0))))

(deftest test-encoded-image->layers
  (is (= [[1 2 3 4 5 6] [7 8 9 0 1 2]]
         (encoded-image->layers [1 2 3 4 5 6 7 8 9 0 1 2] 3 2))))


(deftest test-get-color
  (is (= 0 (get-color [0 2 2 1])))
  (is (= 1 (get-color [2 2 2 1]))))

(deftest test-image-colors
  (is (= [[0 1] [1 0]] (image->colors [0 2 2 2 1 1 2 2 2 2 1 2 0 0 0 0] 2 2))))
