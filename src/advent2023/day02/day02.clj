(ns advent2023.day02.day02)

(require '[clojure.string :as clojure.string])
(require '[clojure.java.io :as clojure.java.io])

;; read input file from a relative path from src/advent2023/day02
(defn read-input [filename]
  (let [path (str "./src/advent2023/day02" filename)]
    (with-open [rdr (clojure.java.io/reader path)]
      (doall (line-seq rdr)))))


;; parse id from Game 1
(defn parse-id [id]
  (Integer/parseInt (last (clojure.string/split id #" "))))

;; parse a set of the form "3 blue, 4 red" into a map of the form {:blue 3 :red 4}
(defn parse-set [set]
  (let [parts (clojure.string/split set #",")]
    (reduce (fn [acc x]
              (let [parts (clojure.string/split (clojure.string/trim x) #" ")]
                (assoc acc (keyword (nth parts 1)) (Integer/parseInt (nth parts 0)))))
            {}
            parts)))


;; parse a line of input into a map of the form {:id 1 :sets [{:blue 1 :green 2 :red 3} {:blue 4 :green 5 :red 6}]}
;; example line Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
(defn parse-line [line]
  (let [parts (clojure.string/split line #":")]
    {:id (parse-id (nth parts 0))
     :sets (map parse-set (clojure.string/split (nth parts 1) #";"))}))

(parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")



(parse-set "3 blue, 4 red")

;; check if a set is valid by ensuring that all colors are less than the constraints
(defn valid-set? [set constraints]
  (every? (fn [[k v]]
            (<= v (get constraints k)))
          set))

(valid-set? {:blue 3 :green 1} {:blue 3 :red 3 :green 2})

;; check if a game is valid by ensuring that all sets are valid
(defn valid-game? [game constraints]
  (every? #(valid-set? % constraints) (:sets game)))

;; return the valid games from the input file with the given constraints
(defn get-valid-games [filename constraints]
  (filter #(valid-game? % constraints) (map parse-line (read-input filename))))

(get-valid-games "/test.txt" {:blue 14 :red 12 :green 13})

;;sum the ids of the valid games
(defn sum-ids [games]
  (reduce + (map :id games)))

(sum-ids (get-valid-games "/test.txt" {:blue 14 :red 12 :green 13}))

(sum-ids (get-valid-games "/input.txt" {:blue 14 :red 12 :green 13}))

;; get the max value for each color in all sets from a parsed game
(defn get-max-colors [game]
  (reduce (fn [acc set]
            (reduce (fn [acc [k v]]
                      (if (nil? (get acc k))
                        (assoc acc k v)
                        (assoc acc k (max v (get acc k)))))
                    acc
                    set))
          {}
          (:sets game)))

(get-max-colors (parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

;; multiply the values of the max colors
(defn multiply-max-colors [game]
  (reduce * (vals (get-max-colors game))))

(multiply-max-colors (parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

(map #(multiply-max-colors (get-max-colors (parse-line %))) (read-input "/test.txt"))

(defn sum [nums]
  (reduce + nums))

(sum (map #(multiply-max-colors (parse-line %)) (read-input "/test.txt")))

(sum (map #(multiply-max-colors (parse-line %)) (read-input "/input.txt")))

(apply + (map #(multiply-max-colors (parse-line %)) (read-input "/test.txt")))