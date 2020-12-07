(ns advent-of-code-2020.day6.question-at-customs
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn question-answered [set-operation input]
  (->> (map #(str/split % #"") (str/split-lines input))
       (map set)
       (apply set-operation)
       count))

(defn count-question-answered [set-operation input]
  (->> (map (partial question-answered set-operation) input)
       (apply +)))

(defn -main [& args]
  (let [input (-> (slurp (io/resource "day6-input.txt"))
                  (str/split #"\n\n"))]

    (println (count-question-answered set/union input))
    (println (count-question-answered set/intersection input))))
