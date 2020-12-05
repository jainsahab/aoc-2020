(ns advent-of-code-2020.day4.valid-passport
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def mandatory-fields #{"iyr" "ecl" "byr" "hcl" "eyr" "hgt" "pid"})

(defn parse-fields [passport]
  (reduce (fn [acc elem]
            (->> (str/split elem #" ")
                 (map #(str/split % #":"))
                 (into acc))) {} passport))

(defn mandatory-fields-present [passport]
  (->> passport
       keys
       set
       (set/difference mandatory-fields)
       count
       (= 0)))

(defn valid-height? [height]
  (cond
    (str/ends-with? height "cm") (-> (subs height 0 (- (count height) 2)) (Integer/parseInt) (#(<= 150 % 193)))
    (str/ends-with? height "in") (-> (subs height 0 (- (count height) 2)) (Integer/parseInt) (#(<= 59 % 76)))))

(defn valid-data-present [passport]
  (and (->> (get passport "byr") (Integer/parseInt) (#(<= 1920 % 2002)))
       (->> (get passport "iyr") (Integer/parseInt) (#(<= 2010 % 2020)))
       (->> (get passport "eyr") (Integer/parseInt) (#(<= 2020 % 2030)))
       (->> (get passport "hgt") valid-height?)
       (->> (get passport "hcl") (re-matches #"^#([a-f0-9]{6})$"))
       (->> (get passport "ecl") (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))
       (->> (get passport "pid") count (= 9))))

(defn passport-with-mandatory-fields [input]
  (->> (map (comp concat str/split-lines) input)
       (map parse-fields)
       (filter mandatory-fields-present)
       count))

(defn passport-with-valid-data [input]
  (->> (map (comp concat str/split-lines) input)
       (map parse-fields)
       (filter mandatory-fields-present)
       (filter valid-data-present)
       count))


(defn -main [& args]
  (let [input (-> (slurp (io/resource "day4-input.txt"))
                  (str/split #"\n\n"))]
    (println (passport-with-mandatory-fields input))
    (println (passport-with-valid-data input))))
