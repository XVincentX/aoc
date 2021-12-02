(ns day2 (:require [clojure.string :refer [split-lines split]]))

(def input (-> "./input/day2input.txt"
               clojure.core/slurp
               split-lines
               (as-> t (map #(split % #" ") t))
               (as-> t (into [] t))))

(def part1
  (reduce
   (fn [position [movement value]]
     (let [iValue (Integer/parseInt value)]
       (condp = movement
         "forward" (update position :horizontal + iValue)
         "down" (update position :depth + iValue)
         "up" (update position :depth - iValue))))
   {:horizontal 0 :depth 0} input))


(defn upd [p val]
  (-> p 
  (update :horizontal + val)
  (update :depth + (* val (:aim p)))))
 
(def part2
  (reduce
   (fn [position [movement value]]
     (let [iValue (Integer/parseInt value)]
       (condp = movement
         "forward" (upd position iValue)
         "down" (update position :aim + iValue)
         "up" (update position :aim - iValue))))
   {:horizontal 0 :depth 0 :aim 0} input))
