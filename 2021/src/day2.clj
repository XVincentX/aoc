(ns day2 (:require [clojure.string :refer [split-lines split]]))

(def input (-> "./input/day2input.txt"
               clojure.core/slurp
               split-lines
               (->> (map #(split % #" "))
                    (map (fn [[x y]] [x (Integer/parseInt y)]))
                    (into []))))

(def part1
  (reduce
   (fn [position [movement value]]
     (condp = movement
       "forward" (update position :horizontal + value)
       "down" (update position :depth + value)
       "up" (update position :depth - value)))
   {:horizontal 0 :depth 0} input))
 
(def part2
  (reduce
   (fn [position [movement value]]
     (condp = movement
       "forward" (-> position
                     (update :horizontal + value)
                     (update :depth + (* value (:aim position))))
       "down" (update position :aim + value)
       "up" (update position :aim - value)))
   {:horizontal 0 :depth 0 :aim 0} input))
