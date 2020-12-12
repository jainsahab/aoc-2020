(ns advent-of-code-2020.day12.rain-risk
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [input]
  {:action (subs input 0 1)
   :value  (Integer/parseInt (subs input 1))})

(defn rotate [directions ship degree]
  (let [times-rotate (/ degree 90)
        directions directions]
    (->> (.indexOf directions (:direction ship))
         (+ times-rotate)
         (#(mod % 4))
         (nth directions)
         (assoc ship :direction))))

(def rotate-right (partial rotate ["E" "S" "W" "N"]))

(def rotate-left (partial rotate ["E" "N" "W" "S"]))

(def forward-direction-map {"E" [:x +]
                            "W" [:x -]
                            "N" [:y +]
                            "S" [:y -]})

(defn move-forward [ship value]
  (->> (:direction ship)
       (get forward-direction-map)
       (#(update ship (first %) (second %) value))))

(defn run-instruction [ship {:keys [action value]}]
  (condp = action
    "N" (update ship :y + value)
    "S" (update ship :y - value)
    "E" (update ship :x + value)
    "W" (update ship :x - value)
    "L" (rotate-left ship value)
    "R" (rotate-right ship value)
    "F" (move-forward ship value)))

(defn simulate-ship-part-1 [ship instructions]
  (reduce run-instruction ship instructions))

(defn manhattan-distance [ship]
  (+ (Math/abs (:x ship)) (Math/abs (:y ship))))


(defn move-forward-part-2 [ship value]
  (let [{:keys [x y]} (:waypoint ship)]
    (-> ship
        (update-in [:ship :x] + (* value x))
        (update-in [:ship :y] + (* value y)))))

(defn rotate-left-part-2 [ship value]
  (let [{:keys [x y]} (:waypoint ship)
        newXY (condp = (mod value 360)
                90 {:x (* -1 y) :y x}
                180 {:x (* -1 x) :y (* -1 y)}
                270 {:x y :y (* -1 x)})]
    (assoc ship :waypoint newXY)))

(defn rotate-right-part-2 [ship value]
  (let [{:keys [x y]} (:waypoint ship)
        newXY (condp = (mod value 360)
                90 {:x y :y (* -1 x)}
                180 {:x (* -1 x) :y (* -1 y)}
                270 {:x (* -1 y) :y x})]
    (assoc ship :waypoint newXY)))

(defn run-instruction-part-2 [ship {:keys [action value]}]
  (condp = action
    "N" (update-in ship [:waypoint :y] + value)
    "S" (update-in ship [:waypoint :y] - value)
    "E" (update-in ship [:waypoint :x] + value)
    "W" (update-in ship [:waypoint :x] - value)
    "L" (rotate-left-part-2 ship value)
    "R" (rotate-right-part-2 ship value)
    "F" (move-forward-part-2 ship value)))

(defn simulate-ship-part-2 [ship instructions]
  (reduce run-instruction-part-2 ship instructions))

(defn manhattan-distance-part-2 [{:keys [ship]}]
  (+ (Math/abs (:x ship)) (Math/abs (:y ship))))

(defn -main [& args]
  (let [input (->> (slurp (io/resource "day12-input.txt"))
                   str/split-lines
                   (map parse-input))]
    (println (manhattan-distance (simulate-ship-part-1 {:direction "E" :x 0 :y 0} input)))
    (println (manhattan-distance-part-2 (simulate-ship-part-2 {:ship     {:x 0 :y 0}
                                                        :waypoint {:x 10 :y 1}} input)))))
