(ns advent2023.day05.day05)

(require '[clojure.string :as clojure.string])
(require '[clojure.java.io :as clojure.java.io])

;; read input file from a relative path from src/advent2023/day05
(defn read-input [filename]
  (let [path (str "./src/advent2023/day05" filename)]
    (with-open [rdr (clojure.java.io/reader path)]
      (doall (line-seq rdr)))))

;; parse a vec of lines like
;; 50 98 2
;; 52 50 48
;; into a vector of maps like [{:destination 50 :source 98 :range 2} {:destination 52 :source 50 :range 48}]
(defn parse-input [input]
  (->> input 
       (map #(let [parts (clojure.string/split % #"\s")]
          {:destination (Long/parseLong (nth parts 0))
           :source (Long/parseLong (nth parts 1))
           :range (Long/parseLong (nth parts 2))}))
       (sort-by :source)))

(defn parse-seeds [input]
  (map #(Long/parseLong %) (clojure.string/split (clojure.string/replace-first input #"seeds: " "") #"\s")))

(defn parse-mappings [input]
  (->>
   (partition-by #(= (clojure.string/trim %) "") input)
   (filter #(not= (first %) ""))
   (map #(vec (parse-input (rest %))))
   vec))

(defn mapping-applies? [input mapping]
  (and (<= (:source mapping) input) (< input (+ (:source mapping) (:range mapping)))))

(defn apply-mapping-2 [input mapping]
  (+ (- input (:source mapping)) (:destination mapping)))

;; find the first mapping where input is >= source and <= source + range, return input - source + destination, if no mapping is found, return input
(defn apply-mapping [input mappings]
  (let [mapping (some #(if (mapping-applies? input %) % nil) mappings)]
    (if mapping
      (apply-mapping-2 input mapping)
      input)))


;; given an input file like
;; seeds: 79 14 55 13

;; seed-to-soil map:
;; 50 98 2
;; 52 50 48

;; soil-to-fertilizer map:
;; 0 15 37
;; 37 52 2
;; 39 0 15

;; turn it into a map like {:seeds [79 14 55 13] :mappings [[{:destination 50 :source 98 :range 2} {:destination 52 :source 50 :range 48}] [{:destination 0 :source 15 :range 37} {:destination 37 :source 52 :range 2} {:destination 39 :source 0 :range 15}]]}
;; support arbitrary mappings
(defn parse [filename]
  (let [input (read-input filename)
        [seeds-input mappings-input] (split-with #(not= (clojure.string/trim %) "") input)
        seeds (parse-seeds (first seeds-input))
        mappings (parse-mappings mappings-input)]
    {:seeds (vec seeds) :mappings mappings}))

(defn apply-mappings [mappings input]
  (reduce apply-mapping  input mappings))

(apply min (let [d (parse "/input.txt")
                 mappings (:mappings d)]
             (map #(apply-mappings mappings %) (:seeds d))))

(defn apply-mapping-to-range [i j m] 
  (let [range-end (+ i j)
        mapping-end (+ (:source m) (:range m))]
    (if (<= range-end mapping-end)
      [[(apply-mapping-2 i m) j] nil]
      [[(apply-mapping-2 i m) (- mapping-end i)] [mapping-end (- range-end mapping-end)]])))

(defn apply-mappings-to-range [range mapping]
  (loop [i (first range)
         j (second range)
         ranges []]
    (let [m (some #(if (mapping-applies? i %) % nil) mapping)]
      (if m
        (let [[r next] (apply-mapping-to-range i j m)]
          (if next
            (recur (first next) (second next) (conj ranges r))
            (conj ranges r)))
        (let [next (some #(if (< i (:source %)) % nil) mapping)]
          (if next
            (if (< (dec (+ i j)) (:source next))
              (conj ranges [i j])
              (recur (:source next) (- (+ i j) (:source next)) (conj ranges [i (- (:source next) i)])))
            (conj ranges [i j])))))))


(defn apply-mappings-to-ranges [ranges mapping]
  (mapcat #(apply-mappings-to-range % mapping) ranges))

(first (sort-by first (let [d (parse "/input.txt"), ranges (map vec (partition 2 (:seeds d)))]
                 (reduce apply-mappings-to-ranges ranges  (:mappings d)))))