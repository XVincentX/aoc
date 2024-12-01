(ns day1 (:require
          [clojure.string :as string]))

(def input (-> "input/1.txt"
               slurp
               string/split-lines))

(defn create-list [acc line] 
  (let [matches (re-seq #"\d+" line)] 
    (-> acc (update :left conj (Integer/parseInt (first matches)))
            (update :right conj (Integer/parseInt (second matches))))))

(defn sum-of-mins [maps] 
  (map (fn [[a b]] (abs (- a b))) 
       (let [sortedLeft (sort (:left maps))
             sortedRight (sort (:right maps))]
         (partition 2 (interleave sortedLeft sortedRight)))))

(def solution1 (->> input
                   (reduce create-list {:left [] :right []})
                   sum-of-mins
                   (apply +)))

(defn multiply-instances [maps]
  (map (fn [elem] 
         (* elem (count (filter (fn [a] (= a elem)) (:right maps))))) 
       (:left maps)))

(def solution2 (->> input
                    (reduce create-list {:left [] :right []})
                    multiply-instances
                    (apply +)))
