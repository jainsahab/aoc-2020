(ns advent-of-code-2020.day5.binary-boarding
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def round-down (comp int #(Math/floor %)))
(def round-up (comp int #(Math/ceil %)))

(defn decode-symbol [input left-sym right-sym]
  (loop [left 0
         right (- (Math/pow 2 (count input)) 1)
         seq input]
    (if (empty? seq)
      left
      (let [center (-> (+ left right)
                       (/ 2))]
        (condp = (first seq)
          left-sym (recur left (round-down center) (rest seq))
          right-sym (recur (round-up center) right (rest seq)))))))

(defn seat-id [input]
  (let [parsed-input (str/split input #"")
        row (decode-symbol (take 7 parsed-input) "F" "B")
        column (decode-symbol (take-last 3 parsed-input) "L" "R")]
    (-> 8
        (* row)
        (+ column))))

(defn find-max-seat-id [input]
  (->> input
       (map seat-id)
       (apply max)))

(defn find-my-seat [input]
  (let [row-with-missing-seat (->> (group-by #(subs % 0 7) input)
                                   (filter #(= 7 (count (second %))))
                                   (into {}))
        row (decode-symbol (-> row-with-missing-seat keys first (str/split #"")) "F" "B")
        all-rows-seat (set (map #(-> 8
                                     (* row)
                                     (+ %)) (range 0 8)))]
    (->> (map seat-id (-> row-with-missing-seat vals first))
         set
         (set/difference all-rows-seat)
         first)))

(defn -main [& args]
  (let [input (->> (slurp (io/resource "day5-input.txt"))
                   str/split-lines)]
    (println (find-max-seat-id input))
    (println (find-my-seat input))))
