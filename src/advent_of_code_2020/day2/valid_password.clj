(ns advent-of-code-2020.day2.valid-password
  (require [clojure.string :as str]
           [clojure.java.io :as io]))


(defn valid-password? [policy password]
  (let [[policy-range policy-character] (str/split policy #" ")
        [min max] (->> (str/split policy-range #"-")
                       (map #(Integer/parseInt %)))
        occurrence-count (-> (filter #(= policy-character (str %)) password)
                  count)]
    (<= min occurrence-count max)))

(defn valid-password-part-two? [policy password]
  (let [[policy-range policy-character] (str/split policy #" ")
        indexes (->> (str/split policy-range #"-")
                       (map #(Integer/parseInt %)))
        first-char (subs password (dec (first indexes)) (first indexes))
        second-char (subs password (dec (last indexes)) (last indexes))]

    (-> (filter #(= policy-character %) [first-char second-char])
        count
        (= 1))))

(defn -main [& args]
  (let [input (-> (slurp (io/resource "day2-input.txt"))
                  (str/split #"\n"))]
    (println (count (filter #(apply valid-password? (str/split % #": ")) input)))
    (println (count (filter #(apply valid-password-part-two? (str/split % #": ")) input)))))
