(ns advent-2020.utils
  (:require [clojure.java.io :as io]))

(defn read-input-file
  "Reads file from resources and returns a line-seq"
  [filename]
  (-> filename
      io/resource
      io/reader
      line-seq))
