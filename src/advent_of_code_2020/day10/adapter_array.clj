(ns advent-of-code-2020.day10.adapter-array
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn product-of-1-and-3-jolt-difference [input]
  (let [max (apply max input)
        device-jolt (+ 3 max)
        input (concat [0] (sort input) [device-jolt])]
    (loop [input input
           acc {1 0 3 0}]
      (if (nil? (next input))
        (->> (select-keys acc [1 3])
             vals
             (apply *))
        (let [first (first input)
              second (second input)]
          (recur (rest input) (update acc (- second first) inc)))))))

(defn distinct-ways [input]
  (let [maximum (apply max input)
        device-jolt (+ 3 maximum)
        input (concat [0] (sort input) [device-jolt])]
    (loop [index 1
           sol {0 1}]
      (if (= index (count input))
        (->> (keys sol)
             (apply max)
             (get sol))
        (let [current-value (nth input index)
              prev-indexes [(dec index) (- index 2) (- index 3)]
              total-ways (->> (filter (complement neg?) prev-indexes)
                              (filter #(<= (- current-value (nth input %)) 3))
                              (map #(get sol %))
                              (apply +))]
          (recur (inc index) (assoc sol index total-ways)))))))

(defn -main [& args]
  (let [input (->> (slurp (io/resource "day10-input.txt"))
                   str/split-lines
                   (map #(Integer/parseInt %)))]
    (println (distinct-ways input))
    (println (product-of-1-and-3-jolt-difference input))))
