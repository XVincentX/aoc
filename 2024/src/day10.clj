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

(defn walk-trail [[[x y] v]]
  (let [down (input [x (dec y)])
        up (input [x (inc y)])
        left (input [(dec x) y])
        right (input [(inc x) y])
        next (inc v)]
    (cond
      (= 9 v) #{[x y]}
      :else (clojure.set/union
             (when (= down next) (walk-trail [[x (dec y)] down]))
             (when (= up next) (walk-trail [[x (inc y)] up]))
             (when (= left next) (walk-trail [[(dec x) y] left]))
             (when (= right next) (walk-trail [[(inc x) y] right]))))))

(defn walk-trail-2 [[[x y] v]]
  (let [down (input [x (dec y)])
        up (input [x (inc y)])
        left (input [(dec x) y])
        right (input [(inc x) y])
        next (inc v)]
    (cond
      (= 9 v) 1
      :else (+
             (if (= down next) (walk-trail-2 [[x (dec y)] down]) 0)
             (if (= up next) (walk-trail-2 [[x (inc y)] up]) 0)
             (if (= left next) (walk-trail-2 [[(dec x) y] left]) 0)
             (if (= right next) (walk-trail-2 [[(inc x) y] right]) 0)))))

(def part1 (apply + (map count (map walk-trail trailheads))))
(def part2  (apply + (map walk-trail-2 trailheads)))
