(ns day8 (:require [clojure.string :refer [split-lines split]]))

(def input (-> "./input/day8input.txt"
               clojure.core/slurp
               split-lines
               (->> (mapv (fn [line] (-> line (split #"\|"))))
                    (mapv (fn [[input output]] [(map set (split input #" ")) (map set (split output #" "))])))))

(def part1 (->> input
                (mapcat second)
                (frequencies)
                (filter (fn [[w _]] (contains? #{2 3 4 7} (count w))))
                (map second)
                (apply +)))

(defn digits-with [input-set n] (->> input-set (filter #(= n (count %))) first))

(defn decode-numbers [input-set]
  (let [one  (digits-with input-set 2)
        four (digits-with input-set 4)
        seven (digits-with input-set 3)
        eight (digits-with input-set 7)

        ns1 (clojure.set/difference input-set #{one} #{four} #{seven} #{eight})
        nine (first (filter #(and (= 6 (count %)) (clojure.set/subset? four %)) ns1))

        ns2 (clojure.set/difference ns1 #{nine})
        zero (first (filter #(and (= 6 (count %)) (clojure.set/subset? one %)) ns2))

        ns3 (clojure.set/difference ns2 #{zero})
        six (first (filter #(= 6 (count %)) ns3))

        ns4 (clojure.set/difference ns3 #{six})
        three (first (filter #(clojure.set/subset? one %) ns4))

        ns5 (clojure.set/difference ns4 #{three})
        five (first (filter #(clojure.set/subset? % six) ns5))

        ns6 (clojure.set/difference ns5 #{five})]

    {zero 0
     one 1
     (first ns6) 2
     three 3
     four 4
     five 5
     six 6
     seven 7
     eight 8
     nine 9}))

(defn to-number [n] 
(let [t (vec n)]
  (reduce-kv
   (fn [tot idx val]
     (+ tot (* val (Math/pow 10 (- 3 idx))))) 0 t)))

(defn decode-output [number-map output]
  (map number-map output))


(def part2 (->> input
                (mapv (fn [[input output]]
                        (-> input
                            set
                            decode-numbers
                            (decode-output (rest output))
                            to-number)))
                (apply +)))
