(ns day9 (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(def input (-> "./input/day9input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(def preamble 25)

(def p1 (loop [index preamble]
          (let [selected-range (take preamble (drop (- index preamble) input))
                input-pair (cartesian-product selected-range selected-range)
                element (nth input index)]
            (if (zero? (count (filter #(= element (+ (first %) (second %))) input-pair)))
              element
              (recur (inc index)))))) ; Part 1

(loop [index 0]
  (let [ret (reduce (fn [[remaining vector] val]
                      (if (= 0 remaining)
                        (reduced (+ (apply min vector) (apply max vector)))
                        (if (> val remaining)
                          (reduced -1)
                          [(- remaining val) (conj vector val)])))
                    [p1 []] (drop index input))]
    (if (= -1 ret) (recur (inc index)) ret)))
