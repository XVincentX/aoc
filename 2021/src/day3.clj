(ns day3 (:require [clojure.string :refer [split-lines split  ]]))

(defn transpose [v]
  (mapv (fn [ind]
          (mapv #(get % ind)
                (filter #(contains? % ind) v)))
        (->> (map count v)
             (apply max)
             range)))

(def input (-> "./input/day3input.txt"
               clojure.core/slurp
               split-lines
               (->> (mapv #(split % #"")))
               (->> (mapv #(mapv (fn [x] (Integer/parseInt x)) %)))))

(def input-freqs (-> input transpose (->> (mapv frequencies))))

(def part1
  (reduce
   (fn [power-data frequencies]
     (let [freq-0 (get frequencies 0)
           freq-1 (get frequencies 1)
           most-common (if (> freq-0 freq-1) 0 1)
           less-common (if (zero? most-common) 1 0)]
       (-> power-data
           (update :gamma str most-common)
           (update :epsilon str less-common))))
   {:gamma "" :epsilon ""} input-freqs))


(defn find-value [default type]
  (loop [ox-data input
         idx 0
         frequencies input-freqs]

(if (= 1 (count ox-data))
  (first ox-data)
  (let [nf (nth frequencies idx)
        freq-0 (get nf 0 0)
        freq-1 (get nf 1 0)
        common {:most (if (> freq-0 freq-1) 0 1) :less (if (> freq-0 freq-1) 1 0)}
        eq-freq (= freq-1 freq-0)
        next (if eq-freq (filter #(= default (nth % idx)) ox-data)
                 (filter #(= (type common) (nth % idx)) ox-data))
        next-freq (-> next transpose (->> (mapv clojure.core/frequencies)))]
    (recur next
           (inc idx)
           next-freq)))))

(def part2
  (let [a (find-value 1 :most)
        b (find-value 0 :less)] [a b]))
