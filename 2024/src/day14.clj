(ns day14
  (:require [clojure.string :as str]))


(def input (->> "src/input/14.txt"
                slurp
                str/split-lines
                (map (partial re-seq #"-?\d+"))
                (map (fn [[x y vx vy]] [[(parse-long x) (parse-long y)] [(parse-long vx) (parse-long vy)]]))))

(def w 101)
(def h 103)

(def limit-w (/ (dec w) 2))
(def limit-h (/ (dec h) 2))

(defn next-tick [[[x y] [vx vy]]]
  (let [next-x (mod (+ x vx) w)
        next-y (mod (+ y vy) h)]
    [[next-x next-y] [vx vy]]))

(def sol1
  (->> input
       (map #(last (take 101 (iterate next-tick %))))
       (map (fn [[[x y]]]
              (cond
                (and (< x limit-w) (< y limit-h)) :q1
                (and (> x limit-w) (< y limit-h)) :q2
                (and (< x limit-w) (> y limit-h)) :q3
                (and (> x limit-w) (> y limit-h)) :q4)))
       frequencies
       (filter #(keyword? (first %)))
       (map second)
       (apply *)))

(defn overlap? [x] 
  (->> x (map first)
       frequencies
       vals
       (some #(not= 1 %))))

(def sol2
  (->> input
       (iterate (fn [x] (map next-tick x)))
       (take-while overlap?)
       count))
