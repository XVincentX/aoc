(ns day4
  (:require [clojure.string :as str]))

(defn indexed [items]
  (map-indexed vector items))

(def input 
  (into {}
        (for [[y line] (indexed (str/split-lines (slurp "src/input/4.txt")))
              [x c]    (indexed line)]
          [[x y] c])))

(defn get-word [grid [x y] [dx dy]]
  (str (get grid [x y])
       (get grid [(+ x dx) (+ y dy)])
       (get grid [(+ x dx dx) (+ y dy dy)])
       (get grid [(+ x dx dx dx) (+ y dy dy dy)])))

(def part1
  (count
   (for [pos (keys input)
         dx [-1 0 1]
         dy [-1 0 1]
         :when (not= 0 dx dy)
         :let [word (get-word input pos [dx dy])]
         :when (= word "XMAS")]
     pos)))


(defn get-mas [grid [x y]]
  (str (get grid [x y])
       (get grid [(- x 1) (- y 1)])
       (get grid [(- x 1) (+ y 1)])
       (get grid [(+ x 1) (+ y 1)])
       (get grid [(+ x 1) (- y 1)])))

(def mas? #{"ASSMM" "ASMMS" "AMMSS" "AMSSM"})

(def part2
  (count
   (for [pos (keys input)
         :let [candidate (get-mas input pos)]
         :when (mas? candidate)]
     pos)))
