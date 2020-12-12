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

(defn count-occupied-seats [input]
  (loop [old input]
    (let [new (apply-rules old)]
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
    (println (time (count-occupied-seats input)))))
