(ns advent-of-code-2020.day7.luggage-processing
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn weighted-node [input]
  (if (= input "no other bags.")
    {}
    (->> (str/split input #" bags?, | bags?.")
         (reduce (fn [acc elem]
                   (let [groups (rest (re-find #"(\d+) (.+)" elem))]
                     (assoc acc (second groups) (Integer/parseInt (first groups))))) {}))))

(defn weighted-graph [input]
  (->> input
       (map #(str/split % #" bags? contain "))
       (reduce #(assoc %1 (first %2) (weighted-node (second %2))) {})))

(defn visited? [node coll]
  (some #(= node %) coll))

(defn neighbours [node graph]
  (-> (get graph node)
      keys))

(defn dfs [src dest graph]
  (loop [stack (vector src)
         visited []]
    (if (empty? stack)
      false
      (let [current-node (peek stack)
            all-neighbours (->> (neighbours current-node graph)
                                (filter (complement #(visited? % visited))))
            new-stack (into (pop stack) all-neighbours)]
        (cond
          (= current-node dest) true
          (visited? current-node visited) (recur new-stack visited)
          :else (recur new-stack (conj visited current-node)))))))


(defn count-bags [graph bag]
  (let [neighbours (get graph bag)]
    (if (empty? neighbours)
      0
      (->> (keys neighbours)
           (map #(+ (get neighbours %) (* (get neighbours %) (count-bags graph %))))
           (apply +)))))

(defn -main [& args]
  (let [input (-> (slurp (io/resource "day7-input.txt"))
                  str/split-lines)
        graf (weighted-graph input)]
    (println (count-bags graf
                         "shiny gold"))
    (println (-> graf
                 (dissoc "shiny gold")
                 keys
                 (#(filter (fn [elem] (dfs elem "shiny gold" graf)) %))
                 count))
    ))
