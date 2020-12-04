(ns advent-of-code-2020.day3.tree-count
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn tree-count [rows right down]
  (loop [rows rows
         tree-count 0
         col 0]
    (if (empty? rows)
      tree-count
      (let [row (str/split (first rows) #"")
            col-count (count row)
            is-tree? (= "#" (nth row col))
            tree-count (if is-tree? (inc tree-count) tree-count)]
        (recur (drop down rows) tree-count (mod (+ col right) col-count))))))

(defn -main [& args]
  (let [input (-> (slurp (io/resource "day3-input.txt"))
                  (str/split #"\n"))]
    (println (tree-count input 3 1))

    (let [slopes [[1 1] [3 1] [5 1]
                  [7 1] [1 2]]]
      (println (apply * (map #(tree-count input (first %) (second %)) slopes))))))
