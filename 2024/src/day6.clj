(ns day6
  (:require [clojure.string :as str]))

(defn indexed [items]
  (map-indexed vector items))

(def input
  (into {}
        (for [[y line] (indexed (str/split-lines (slurp "src/input/6.txt")))
              [x c]    (indexed line)]
          [[x y] c])))

(def next-dir
  {\^ [0 -1]
   \> [1 0]
   \< [-1 0]
   \v [0 1]})

(def rotate
  {\^ \>
   \> \v
   \< \^
   \v \<})

; start dir [42 83] [4 6]
(def guardian-starting-point [42 83])

(defn walk-grid [input]
  (loop [grid input
         [x y] guardian-starting-point
         cnt 0
         visited-points #{}]
    (let [guardian (grid [x y])
          [dx dy] (next-dir guardian)
          next-point [(+ x dx) (+ y dy)]
          next-slot (grid next-point -1)
          next-grid (assoc grid next-point guardian [x y] \X)
          next-visited-points (conj visited-points [x y guardian])]
      (when-not (visited-points [x y guardian])
        (condp = next-slot
          -1  [(inc cnt) visited-points]
          \. (recur next-grid next-point (inc cnt) next-visited-points)
          \X (recur next-grid next-point cnt next-visited-points)
          \# (recur (assoc grid [x y] (rotate guardian)) [x y] cnt next-visited-points))))))

(def sol1 (walk-grid input))

(defn sol2 [] 
  (let [[_ visited-points] (walk-grid input)] 
    (for [[x y] visited-points
          :let [attempt-grid (assoc input [x y] \#)
                solution (if (not= guardian-starting-point [x y]) (walk-grid attempt-grid) nil)]
          :when (nil? solution)]
      [x y])))

(count (distinct (sol2)))
