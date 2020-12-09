(ns advent-of-code-2020.day8.infinite-loop
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn acc-at-cycle [input]
  (loop [index 0
         accumulator 0
         visited-index #{}]
    (if (contains? visited-index index)
      accumulator
      (let [[instruction val] (str/split (nth input index) #" ")]
        (condp = instruction
          "acc" (recur (inc index) (+ accumulator (Integer/parseInt val)) (conj visited-index index))
          "jmp" (recur (+ index (Integer/parseInt val)) accumulator (conj visited-index index))
          "nop" (recur (inc index) accumulator (conj visited-index index)))))))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn alter-statement [input]
  (if (str/starts-with? input "nop")
    (str/replace input #"nop" "jmp")
    (str/replace input #"jmp" "nop")))

(defn acc-at-termination [input]
  (let [index-to-alter (indices #(or (str/starts-with? % "nop") (str/starts-with? % "jmp")) input)
        inputs (map #(concat (subvec input 0 %) [(alter-statement (nth input %))] (subvec input (inc %))) index-to-alter)]

    (doseq [input inputs]
      (loop [index 0
             accumulator 0
             visited-index #{}]
        (cond
          (= (count input) index) (println "EXECUTED SUCCESSFULLY" accumulator)
          (contains? visited-index index) (println "CYCLE FOUND" accumulator)
          :else (let [[instruction val] (str/split (nth input index) #" ")]
                  (condp = instruction
                    "acc" (recur (inc index) (+ accumulator (Integer/parseInt val)) (conj visited-index index))
                    "jmp" (recur (+ index (Integer/parseInt val)) accumulator (conj visited-index index))
                    "nop" (recur (inc index) accumulator (conj visited-index index)))))))))

(defn -main [& args]
  (let [input (-> (slurp (io/resource "day8-input.txt"))
                  str/split-lines)]
    (acc-at-termination input)
    (println (acc-at-cycle input))))
