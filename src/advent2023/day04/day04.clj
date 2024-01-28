(ns advent2023.day04.day04)

(require '[clojure.java.io :as clojure.java.io])
(require '[clojure.string :as clojure.string])
(require '[clojure.set :as clojure.set])

;; read input file from a relative path from src/advent2023/day04
(defn read-input [filename]
  (let [path (str "./src/advent2023/day04" filename)]
    (with-open [rdr (clojure.java.io/reader path)]
      (doall (line-seq rdr)))))

;; parse the int out of a string like "Card 1"
(defn parse-id [id]
  (Integer/parseInt (last (clojure.string/split id #" "))))

;; parse the set of cards from an input "83 86  6 31 17  9 48 53" into a set of integers
(defn parse-set [input]
  (set (map #(Integer/parseInt %)
            (filter not-empty
                    (clojure.string/split (clojure.string/trim input) #"\s")))))

;; parse the line of input like "41 48 83 86 17 | 83 86  6 31 17  9 48 53" into a map of the form {:hand1 set1 :hand2 set2}
(defn parse-hands [hands]
  (let [parts (clojure.string/split hands #"\|")]
    {:hand1 (parse-set (nth parts 0))
     :hand2 (parse-set (nth parts 1))}))

;; parse a line of input like `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53` into a map of the form {:id 1 :hand1 set1 :hand2 set2}
(defn parse-line [line]
  (let [parts (clojure.string/split line #":")]
    (merge {:id (parse-id (nth parts 0))} (parse-hands (nth parts 1)))))

(defn get-match-count [round]
  (count (clojure.set/intersection (:hand1 round) (:hand2 round))))

;; get a hands score by getting the number of cards present in both hands and rasing 2 to that power minus 1
(defn get-score [round]
  (let [size (get-match-count round)]
    (if (> size 0)
      (Math/pow 2 (dec size))
      0)))

;; for each line of input, parse the line and get the score for that round, then sum the scores
(defn get-total-score [filename]
  (reduce + (map get-score (map parse-line (read-input filename)))))

(get-total-score "/input.txt")

(defn increment-counts [counts start end val]
  (loop [i start
         counts counts]
    (if (> i end)
      counts
      (recur (inc i) (assoc counts i (+ (counts i) val))))))

(defn run-game [filename]
  (let [rounds (map parse-line (read-input filename))]
    (loop [i 0
           counts (vec (repeat (count rounds) 1))]
      (if (= i (count rounds)) counts
          (let [match-count (get-match-count (nth rounds i))]
            (recur (+ i 1) (increment-counts counts (inc i) (+ i match-count) (nth counts i))))))))


(apply + (run-game "/input.txt"))
