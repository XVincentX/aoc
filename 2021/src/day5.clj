(ns day5 (:require [clojure.string :refer [split-lines split]]
                   [clojure.math.combinatorics :as combo]))


(def input (-> "./input/day5input.txt"
               clojure.core/slurp
               split-lines
               (->> (mapv (fn [line] (-> line
                                          (split #" -> ")
                                          (->> (mapv #(mapv clojure.core/parse-long (split % #","))))))))))

(def grid-size (->> input
                (map (fn [[[x1 _] [x2 _]]] (max x1 x2)))
                (apply max)
                inc))

(def matrix (vec (repeat grid-size (vec (repeat grid-size 0)))))

(defn compute-tuples [diagonal? [[x1 y1] [x2 y2]]]
  (let [mmx (inc (max x1 x2))
        mix (min x1 x2)
        gt_x (> x1 x2)

        mmy (inc (max y1 y2))
        miy (min y1 y2)
        gt_y (> y1 y2)

        x-range (range mix mmx)
        y-range (range miy mmy)]
    (if (or (= x1 x2)
            (= y1 y2))
      (combo/cartesian-product (if (empty? x-range) [x1] x-range)
                               (if (empty? y-range) [y1] y-range))
      (if diagonal? (partition 2 (interleave (if gt_x x-range (reverse x-range))
                               (if gt_y y-range (reverse y-range)))) []))))

(defn trav [tuples-fn input]
  (->> (reduce (fn [matrix [[x1 y1] [x2 y2]]]
                 (reduce (fn [matrix [x y]]
                           (-> matrix
                               (update y update x inc))) matrix
                         (tuples-fn [[x1 y1] [x2 y2]])))
               matrix input)
       (mapcat identity)
       (filter #(>= % 2))
       count))

(def part1 (trav (partial compute-tuples false) input))
(def part2 (trav (partial compute-tuples true) input))
