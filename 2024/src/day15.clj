(ns day15
  (:require [clojure.string :as str]
            [pp-grid.api :as g]))

(defn indexed [items]
  (map-indexed vector items))

(def input
  (into {}
        (for [[y line] (indexed (str/split-lines (first (str/split (slurp "src/input/15.txt") #"\n\n"))))
              [x c]    (indexed line)]
          [[x y] c])))

(def txt-2 (-> "src/input/15.txt"
               slurp
               (str/split #"\n\n")
               first
               (str/replace #"#" "##")
               (str/replace #"O" "[]")
               (str/replace #"\." "..")
               (str/replace #"@" "@.")))

(def input-2
  (into {}
        (for [[y line] (indexed (str/split-lines txt-2))
              [x c]    (indexed line)]
          [[x y] c])))

(def ops (second (str/split (slurp "src/input/15.txt") #"\n\n")))

(def dirs
  {\^ [0 -1]
   \> [1 0]
   \< [-1 0]
   \v [0 1]})

(defn find-robot [grid]
  (->> grid
       (filter (fn [[_ v]] (= \@ v)))
       ffirst))

(declare can-move?)

(defn can-move-int? [grid [x y] [dx dy :as dir]] 
  (let [next-point [(+ x dx) (+ y dy)]
        next (grid next-point)]
    (condp = next
      \. true
      \# false
      (can-move? grid next-point dir))))

(defn can-move? [grid [x y] [dx dy :as dir]]
  (let [[nx ny :as next-point] [(+ x dx) (+ y dy)]
        next (grid next-point)]
    (condp = next
      \. true
      \# false
      \O (can-move? grid next-point dir)
      (let [[ox oy] (if (= \[ next) [(inc nx) ny] [(dec nx) ny])]
        (and
         (can-move? grid next-point [dx dy])
         (can-move-int? grid [ox oy] [dx dy]))))))

(defn move-piece [grid [x y :as pos] [dx dy :as dir]] 
  (let [[nx ny :as next-point] [(+ x dx) (+ y dy)]
        next (grid next-point)]
    (condp = next
      \. (assoc grid pos \. next-point (grid pos))
      \# grid
      \O (let [next-grid (move-piece grid next-point dir)]
           (condp = (next-grid next-point)
             \. (assoc next-grid pos \. next-point (grid pos))
             \O next-grid))
      (if (can-move? grid pos dir)
        (let [[ox oy] (if (= \[ next) [(inc nx) ny] [(dec nx) ny])
              ng1 (move-piece grid next-point dir)
              ng2 (move-piece ng1 [ox oy] dir)
              next-grid (if (or (zero? dy) (= grid ng1))
                          ng1
                          (if (= ng1 ng2) grid ng2))]
          (condp = (next-grid next-point)
            \. (assoc next-grid
                      pos \.
                      next-point (grid pos))
            next-grid))
        grid))))


(defn sol [i char]
  
  (let [final-grid (reduce (fn [grid op] (move-piece grid (find-robot grid) (dirs op))) i ops)
        boxes (filter (fn [[_ v]] (= char v)) final-grid)
        boxes-coords (keys boxes)] 
    (reduce (fn [acc [x y]]
              (+ acc (+ (* 100 y) x))) 0 boxes-coords)))

 (def sol1 (sol input \O))
(def sol2 (sol input-2 \[))

