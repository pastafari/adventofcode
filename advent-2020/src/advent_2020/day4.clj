(ns advent-2020.day4
  (:require [advent-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))


(defn between-cm
  "If height in cm, returns whether between at-least, at-most, else true"
  [at-least at-most]
  (fn [height-str]
    (let [[_ height] (re-find #"^(\d+)cm$" height-str)]
      (when height
        (let [height-int (Integer/parseInt height)]
          (and (>= height-int at-least)
               (<= height-int at-most)))))))

(defn between-in
  "If height in inches, returns whether between at-least, at-most, else true"
  [at-least at-most]
  (fn [height-str]
    (let [[_ height] (re-find #"^(\d+)in$" height-str)]
      (when height
        (let [height-int (Integer/parseInt height)]
          (and (>= height-int at-least)
               (<= height-int at-most)))))))

(s/def ::year #(re-find #"^\d{4}$" %))
(s/def ::nine-digits #(re-find #"^\d{9}$" %))
(s/def ::height #(re-find #"^\d+(cm|in)$" %))
(s/def ::byr (s/and #(s/valid? ::year %)
                    #(<= (.compareTo "1920" %) 0)
                    #(>= (.compareTo "2002" %) 0)))
(s/def ::iyr (s/and #(s/valid? ::year %)
                    #(<= (.compareTo "2010" %) 0)
                    #(>= (.compareTo "2020" %) 0)))
(s/def ::eyr (s/and #(s/valid? ::year %)
                    #(<= (.compareTo "2020" %) 0)
                    #(>= (.compareTo "2030" %) 0)))
(s/def ::hgt (s/and #(s/valid? ::height %)
                    (fn [h]
                      (or ((between-cm 150 193) h)
                          ((between-in 59 76) h)))))
(s/def ::hcl #(re-find #"(^#((\p{Lower}|\p{Digit}){6}))$" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid ::nine-digits)
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


(defn day4-2
  "Reads file, counts valid passports"
  [file]
  (->> file
      utils/read-input-file
      make-passports
      (filter #(s/valid? ::passport %))
      count))


(deftest valid-passport-test
  (testing "all invalid passports"
    (is (false? (s/valid? ::passport
                          (make-passport ["eyr:1972 cid:100 hcl:#18171d
ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"]))))
    (is (false? (s/valid? ::passport
                          (make-passport ["iyr:2019
                                       hcl:#602927 eyr:1967 hgt:170cm
                                       ecl:grn pid:012533040 byr:1946"]))))
    (is (false? (s/valid? ::passport
                          (make-passport ["hcl:dab227 iyr:2012
    ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
"]))))
    (is (false? (s/valid? ::passport
                          (make-passport ["    hgt:59cm ecl:zzz
    eyr:2038 hcl:74454a iyr:2023
    pid:3556412378 byr:2007"])))))
  (testing "all valid passports"
    (let [lines ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                          "hcl:#623a2f"
                          ""
                          "eyr:2029 ecl:blu cid:129 byr:1989"
                          "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                          ""
                          "hcl:#888785"
                          "hgt:164cm byr:2001 iyr:2015 cid:88"
                          "pid:545766238 ecl:hzl"
                          "eyr:2022"
                          ""
                          "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
                          ]
          passports (make-passports lines)]
      (is (every? true? (map #(s/valid? ::passport %) passports))))))
