(ns adventofcode2023.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-1 [s]
  (re-seq #"\d" s))

(def numbers ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def number-pattern (re-pattern (str (s/join "|" numbers) "|\\d")))
(def number-lookup (zipmap numbers (rest (range))))

(defn subs-matches [re s]
  (loop [s s
         res []]
    (if (seq s)
      (let [m (re-matcher re s)]
        (if (.find m)
          (recur (subs s (inc (.start m))) (conj res (.group m)))
          (recur (subs s 1) res)))
      res)))

(comment
  (= ["eight" "three"] (subs-matches number-pattern "eighthree"))
  (= ["eight" "three"] (subs-matches number-pattern "eightthree"))
  (= ["eight" "three" "5"] (subs-matches number-pattern "eighthree5"))
  (= ["one" "eight"] (subs-matches number-pattern "butoneight")))

(defn parse-2 [s]
  (->> (subs-matches number-pattern s)
       (map #(or (get number-lookup %) (parse-long %)))))

(defn get-numbers [parse-f lines]
  (->> lines
       (map parse-f)
       (map #(str (first %) (last %)))
       (map parse-long)))

(defn solve [parse-f lines]
  (->> lines
       (get-numbers parse-f)
       (reduce + 0)))

(comment
  (with-open [r (io/reader (io/resource "adventofcode2023/day_1.txt"))]
    (= 54916 (solve parse-1 (line-seq r))))

  (= 142 (solve parse-1 ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]))

  (with-open [r (io/reader (io/resource "adventofcode2023/day_1.txt"))]
    (= 54728 (solve parse-2 (line-seq r))))


  (= 281 (solve parse-2 ["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"]))

  (with-open [r (io/reader (io/resource "adventofcode2023/day_1.txt"))]
    (->> (line-seq r)
         (get-numbers parse-2)
         (take 10)
         (doall)))


  (= (parse-2 "onetwothree") [1 2 3])
  (= (parse-2 "onetwthrfoboomurfour") [1 4])
  (= (parse-2 "zerobsdf4zieight2") [4 8 2])
  (= (parse-2 "onetwt6hrfoboomurfour") [1 6 4])
  (= (parse-2 "123") [1 2 3])
  (= (parse-2 "eighthree") [8 3]))
