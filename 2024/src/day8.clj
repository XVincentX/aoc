(ns day8
  (:require [clojure.string :as str]))

(defn indexed [items]
  (map-indexed vector items))

(def input
  (into {}
        (for [[y line] (indexed (str/split-lines (slurp "src/input/8.txt")))
              [x c]    (indexed line)]
          [[x y] c])))

(defn make-node-map [input]
  (reduce #(merge-with into %1 %2)
          (for [[k v] input
                :when (not= \. v)]
            {v [k]})))

(defn possible-antinodes [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [[(- x1 dx) (- y1 dy)]
     [(+ x2 dx) (+ y2 dy)]]))

(defn pairs [xs]
  (for [x1 xs
        x2 xs
        :when (= 1 (compare x1 x2))]
    [x1 x2]))

(def part1
  (count
   (set
    (let [node-map (make-node-map input)]
      (for [[_ positions] node-map
            [p1 p2]       (pairs positions)
            p3            (possible-antinodes p1 p2)
            :when         (contains? input p3)]
        p3)))))

(defn bigger-antinodes [input [x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (concat
     (take-while input (iterate #(mapv - % [dx dy]) [x1 y1]))
     (take-while input (iterate #(mapv + % [dx dy]) [x2 y2])))))


(def part2
  (count
   (set
    (for [[_ positions] (make-node-map input)
          [p1 p2]       (pairs positions)
          p3            (bigger-antinodes input p1 p2)
          :when         (contains? input p3)]
      p3))))
