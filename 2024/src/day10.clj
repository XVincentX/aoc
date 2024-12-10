(ns day10
  (:require [clojure.string :as str]
            [clojure.set]))

(defn indexed [items]
  (map-indexed vector items))

(def input
  (into {}
        (for [[y line] (indexed (str/split-lines (slurp "src/input/10.txt")))
              [x c]    (indexed line)]
          [[x y] (Character/getNumericValue c)])))

(def trailheads (filter (fn [[_ c]] (zero? c)) input))

(defn walk-trail [f val [[x y] v]]
  (let [down (input [x (dec y)])
        up (input [x (inc y)])
        left (input [(dec x) y])
        right (input [(inc x) y])
        next (inc v)]
    (cond
      (= 9 v) (if (zero? val) #{[x y]} val)
      :else (f
             (when (= down next) (walk-trail f val [[x (dec y)] down]))
             (when (= up next) (walk-trail f val [[x (inc y)] up]))
             (when (= left next) (walk-trail f val [[(dec x) y] left]))
             (when (= right next) (walk-trail f val [[(inc x) y] right]))))))

(def part1 (apply + (map count (map (partial walk-trail clojure.set/union 0) trailheads))))
(def part2  (apply + (map (partial walk-trail + 1) trailheads)))
