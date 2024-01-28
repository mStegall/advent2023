(ns advent2023.day03.day03)

(require '[clojure.string :as clojure.string])
(require '[clojure.java.io :as clojure.java.io])


;; read input file from a relative path from src/advent2023/day03 into a vector of vectors of characters
(defn read-input [filename]
  (let [path (str "./src/advent2023/day03" filename)]
    (with-open [rdr (clojure.java.io/reader path)]
      (doall (mapv #(vec %) (line-seq rdr))))))

;; get coordinates for all characters that aren't . or a digit
(defn get-coordinates [grid]
  (let [height (count grid)
        width (count (first grid))]
    (for [y (range height)
          x (range width)
          :when (not (or (= (get-in grid [y x]) \.)
                         (Character/isDigit (get-in grid [y x]))))]
      [x y])))

;; get gear potentials, a gear is represented with *
(defn get-gear-potentials [grid]
  (let [height (count grid)
        width (count (first grid))]
    (for [y (range height)
          x (range width)
          :when (= (get-in grid [y x]) \*)]
      [x y])))

;; given a coordinate and a grid, find the coordinate of the first numeric character directly adjacent to the given coordinate, including diagonals
(defn get-adjacent [grid [x y]]
  (let [height (count grid)
        width (count (first grid))
        x- (dec x)
        x+ (inc x)
        y- (dec y)
        y+ (inc y)]
    (->> (list [x- y-] [x y-] [x+ y-]
               [x- y]          [x+ y]
               [x- y+] [x y+] [x+ y+])
         (filter (fn [[x y]]
                   (and (>= x 0) (< x width)
                        (>= y 0) (< y height))))
         (filter (fn [[x y]]
                   (Character/isDigit (get-in grid [y x])))))))

;; get the number in the grid by starting at a given coordinate and following the path of adjacent digits both left and right
(defn get-number-start [grid [x y]]
  (loop [x x]
    (if (and (> x 0) (Character/isDigit (get-in grid [y (dec x)])))
      (recur (dec x))
      [x y])))

(defn read-number-at [grid [x y]]
  (loop [x x
         number ""]
    (if (and (< x (dec (count (get-in grid [y])))) (Character/isDigit (get-in grid [y (inc x)])))
      (recur (inc x) (str number (get-in grid [y x])))
      (Integer/parseInt (str number (get-in grid [y x]))))))

(defn get-adjacent-numbers [grid [x y]]
  (->> (get-adjacent grid [x y])
       (map #(get-number-start grid %))
       (map #(read-number-at grid %))
       (distinct)))

;; get all numbers starts adjacent to a symbol and get unique coordinates
(defn get-unique-numbers [grid]
  (let [coordinates (get-coordinates grid)]
    (println coordinates)
    (->> coordinates
         (mapcat #(get-adjacent grid %))
         (map #(get-number-start grid %))
         (distinct)
         (map #(read-number-at grid %))
         )))

(apply + (get-unique-numbers (read-input "/input.txt")))

(defn get-gears [grid]
  (->> (get-gear-potentials grid)
       (map #(get-adjacent-numbers grid %))
       (filter #(= (count %) 2))
       (map #(apply * %))))

(apply + (get-gears (read-input "/input.txt")))
