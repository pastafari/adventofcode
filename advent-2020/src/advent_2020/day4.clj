(ns advent-2020.day4
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]))


(s/def ::byr some?)
(s/def ::iyr some?)
(s/def ::eyr some?)
(s/def ::hgt some?)
(s/def ::hcl some?)
(s/def ::ecl some?)
(s/def ::pid some?)
(s/def ::cid some?)

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))


;; Thanks Mark Needham
;; https://markhneedham.com/blog/2013/09/22/clojure-stripping-all-the-whitespace/
(defn split-on
  [re s]
  (str/split s re))

(defn trim-extra-spaces
  [line]
  (->> line
       (split-on #"\s")
       (filter #(not (str/blank? %)))
       (str/join " ")))

(defn make-passport
  "Takes line(s) belong to 1 passport, makes passport"
  [lines]
  (let [line (str/join " " (map trim-extra-spaces lines))]
    (->> line
        (split-on #"\s")
        (map #(split-on #":" %))
        (into {})
        (walk/keywordize-keys))))

(defn make-passports
  "Reads seq of lines, partitons-by empty lines, removes them, makes passports"
  [lines]
  (->> lines
       (partition-by #{""})
       (filter #(not (= '("") %)))
       (map make-passport)))


(defn day4-1
  "Reads file, counts valid passports"
  [file]
  (->> file
      utils/read-input-file
      make-passports
      (filter #(s/valid? ::passport %))
      count))
