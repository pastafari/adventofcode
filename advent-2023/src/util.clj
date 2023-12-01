(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-seq
  "Reads file from resources and returns a line-seq"
  [file]
  (-> file
      io/resource
      io/reader
      line-seq))

(defn csv->ints
  "Converts \"1,2,3\" into [1 2 3]"
  [x]
  (mapv #(Integer/parseInt %)
        (str/split x #",")))
