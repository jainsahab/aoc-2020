(ns advent-of-code-2020.day11.seating-space
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn adjacent-seats [input row col]
  (let [pairs (vec (for [f1 [dec (constantly row) inc]
                         f2 [dec (constantly col) inc]
                         :let [r (f1 row)
                               c (f2 col)
                               _ (> (count input) r)]
                         :when (and (<= 0 r) (> (count input) r)
                                    (<= 0 c) (> (count (first input)) c))]
                     [r c]))]
    (->> pairs
         (filter #(not= % [row col]))
         (map #(-> (nth input (first %))
                   (nth (second %)))))))

(defn index-in-direction [rfn cfn row col]
  (lazy-seq (cons [(rfn row) (cfn col)] (index-in-direction rfn cfn (rfn row) (cfn col)))))
(def top (partial index-in-direction dec identity))
(def bottom (partial index-in-direction inc identity))
(def left (partial index-in-direction identity dec))
(def right (partial index-in-direction identity inc))
(def top-left (partial index-in-direction dec dec))
(def top-right (partial index-in-direction dec inc))
(def bottom-left (partial index-in-direction inc dec))
(def bottom-right (partial index-in-direction inc inc))

(defn find-first [f coll]
  (first (filter f coll)))

(defn seats-in-all-direction [input row col]
  (for [direction [top bottom left right top-left top-right bottom-left bottom-right]
        :let [value (->> (direction row col)
                         (find-first #(not= "." (get-in input %))))]
        :when (not (nil? (get-in input value)))]
    (get-in input value)))

(defn apply-rules-unoptimized [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))]
         (let [adjacent-frequencies (-> (adjacent-seats input row col)
                                        frequencies)
               current (-> (nth input row)
                           (nth col))]
           (cond
             (and (= current "L") (= 0 (get adjacent-frequencies "#" 0))) "#"
             (and (= current "#") (<= 4 (get adjacent-frequencies "#" 0))) "L"
             :else current)))
       (partition (count (first input)) (count (first input)))))

(defn change-seat-availability [seat]
  (if (= seat "L") "#" "L"))

(defn apply-rules [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))
             :let [adjacent-frequencies (-> (adjacent-seats input row col)
                                            frequencies)
                   current (-> (nth input row)
                               (nth col))]
             :when (or (and (= current "L") (= 0 (get adjacent-frequencies "#" 0)))
                       (and (= current "#") (<= 4 (get adjacent-frequencies "#" 0))))]
         [row col])
       (reduce #(update-in %1 %2 change-seat-availability) input)))

(defn apply-rules-part-two [input]
  (->> (for [row (range (count input))
             col (range (count (first input)))
             :let [adjacent-frequencies (-> (seats-in-all-direction input row col)
                                            frequencies)
                   current (-> (nth input row)
                               (nth col))]
             :when (or (and (= current "L") (= 0 (get adjacent-frequencies "#" 0)))
                       (and (= current "#") (<= 5 (get adjacent-frequencies "#" 0))))]
         [row col])
       (reduce #(update-in %1 %2 change-seat-availability) input)))

(defn count-occupied-seats [fn input]
  (loop [old input]
    (let [new (fn old)]
      (if (= old new)
        (->> new
             (map frequencies)
             (map #(get % "#" 0))
             (apply +))
        (recur new)))))

(defn -main [& args]
  (let [input (->> (slurp (io/resource "day11-input.txt"))
                   str/split-lines
                   (map #(str/split % #""))
                   vec)]
    (println (time (count-occupied-seats apply-rules input)))
    (println (time (count-occupied-seats apply-rules-part-two input)))))
