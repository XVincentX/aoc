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
               (->> (mapv #(mapv (fn [x] clojure.core/parse-long) %)))))

(def input-freqs (-> input transpose (->> (mapv frequencies))))

(def part1
  (reduce
   (fn [power-data frequencies]
     (let [freq-0 (get frequencies 0)
           freq-1 (get frequencies 1)
           most-common (if (> freq-0 freq-1) 0 1)]
       (-> power-data
           (update :gamma str most-common)
           (update :epsilon str (bit-xor most-common 1)))))
   {:gamma "" :epsilon ""} input-freqs))


(defn find-value [default]
  (loop [ox-data input
         idx 0
         frequencies input-freqs]

(if (= 1 (count ox-data))
  (first ox-data)
  (let [nf (nth frequencies idx)
        freq-0 (get nf 0 0)
        freq-1 (get nf 1 0)
        eq-freq (= freq-0 freq-1)
        most-common (if (> freq-0 freq-1) 0 1)
        filter-val (if eq-freq default (if (zero? default) (bit-xor most-common 1) most-common))
        next (filter #(= filter-val (nth % idx)) ox-data)]
        
    (recur next
           (inc idx)
           (-> next transpose (->> (mapv clojure.core/frequencies))))))))

(def part2 {:oxygen (find-value 1) :co2 (find-value 0)})
