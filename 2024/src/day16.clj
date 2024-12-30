(ns day16
  (:require [clojure.string :as str]
            [clojure.set]))

(defn indexed [items]
  (map-indexed vector items))

(def input
  (into {}
        (for [[y line] (indexed (str/split-lines (first (str/split (slurp "src/input/16.txt") #"\n\n"))))
              [x c]    (indexed line)]
          [[x y] c])))

(def dirs #{[0 1] [1 0] [0 -1] [-1 0]})

(defn start-point [grid] (ffirst (filter (fn [[_ v]] (= \S v)) grid)))

(defn opposite [direction] (map #(- %) direction))

(defn next-step [grid sum [x y] direction visited]
  (for [[dx dy] (clojure.set/difference dirs  #{(opposite direction)})
        :let [next-point [(+ x dx) (+ y dy)]]
        :when (and (not= (grid next-point) \#)
                   (nil? (visited [x y])))]
    (if (= \E (grid next-point))
      (inc sum)
      (next-step grid (if (= direction [dx dy]) (inc sum) (+ 1001 sum)) next-point [dx dy] (conj visited [x y])))))

 (println (apply min (flatten (next-step input 0 (start-point input) [1 0] #{}))))
