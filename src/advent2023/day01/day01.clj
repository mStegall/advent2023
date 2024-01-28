(ns advent2023.day01.day01
  (:gen-class))

(require '[clojure.string :as clojure.string])
(require '[clojure.java.io :as clojure.java.io])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; read input file from a relative path from src/advent2023/day01
(defn read-input [filename]
  (let [path (str "./src/advent2023/day01" filename)]
    (with-open [rdr (clojure.java.io/reader path)]
      (doall (line-seq rdr)))))

(read-input "/test.txt")

;; filter a string to only numeric characters
(defn filter-numeric [s]
  (clojure.string/replace s #"[^0-9]" ""))

;; get the first and last character of a string, combine them, and convert to int
(defn get-first-and-last [s]
  (let [first (first s)
        last (last s)]
    (Integer/parseInt (str first last))))

;; replace spelled out numbers with digits in a string and return the modified string, replacements should be processed one character at a time
;; eightwothree -> 8wo3 
;; zoneight234 -> z1ight234
(defn replace-spelled-out-numbers [s]
  (-> s
      (clojure.string/replace "one" "one1one")
      (clojure.string/replace "two" "two2two")
      (clojure.string/replace "three" "three3three")
      (clojure.string/replace "four" "four4four")
      (clojure.string/replace "five" "five5five")
      (clojure.string/replace "six" "six6six")
      (clojure.string/replace "seven" "seven7seven")
      (clojure.string/replace "eight" "eight8eight")
      (clojure.string/replace "nine" "nine9nine")
      (clojure.string/replace "zero" "zero0zero")))

;; map over the input, filter out non-numeric characters, and convert to numbers
(defn get-numbers [input]
  (map #(get-first-and-last (filter-numeric %)) input))

(defn get-numbers-2 [input]
  (map #(get-first-and-last (filter-numeric (replace-spelled-out-numbers %))) input))

(get-numbers (read-input "/test.txt"))

;; sum the numbers
(defn sum-numbers [nums]
  (reduce + nums))

(sum-numbers (get-numbers (read-input "/test.txt")))

(sum-numbers (get-numbers (read-input "/input.txt")))

(sum-numbers (get-numbers-2 (read-input "/test2.txt")))

(sum-numbers (get-numbers-2 (read-input "/input.txt")))

