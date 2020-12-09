(ns advent-of-code-2020.day9.encoding-error
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-sum-pair [list n]
  (loop [s (sort list)]
    (if (< (count s) 2)
      []
      (let [sum (+ (first s) (last s))]
        (if (= sum n)
          [(first s) (last s)]
          (if (< n sum)
            (recur (butlast s))
            (recur (rest s))))))))

(defn find-first-non-match [input n]
  (loop [preamble (vec (take n input))
         list (subvec input n)]
    (let [num (first list)
          sum-pair (get-sum-pair preamble num)]
      (if (empty? sum-pair)
        num
        (recur (conj (subvec preamble 1) num) (rest list))))))

(defn increase-start-until-less-than-sum [start input n acc]
  (loop [start start
         acc acc]
    (if (<= acc n)
      [start acc]
      (recur (inc start) (- acc (nth input start))))))

(defn get-sum-range [input n]
  (loop [start 0
         end 0
         acc 0]
    (println start end acc)
    (if (= acc n)
      (let [contiguous-sub-range (subvec input start end)
            minimum (apply min contiguous-sub-range)
            maximum (apply max contiguous-sub-range)]
        (+ minimum maximum))
      (let [sum (+ acc (nth input end))]
        (if (< sum n)
          (recur start (inc end) sum)
          (let [[start acc] (increase-start-until-less-than-sum start input n sum)]
            (recur start (inc end) acc)))))))

(defn -main [& args]
  (let [input (->> (slurp (io/resource "day9-input.txt"))
                   str/split-lines
                   (map #(Long/parseLong %))
                   vec)]
    (println (get-sum-range input 36845998))
    (println (find-first-non-match input 25))))
