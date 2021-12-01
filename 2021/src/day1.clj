(ns day4 (:require [clojure.string :refer [split-lines]]))


(def input (into [] (map #(. Integer parseInt %) (split-lines (clojure.core/slurp "./input/day1input.txt")))))

(def part1 (reduce
            (fn [{:keys [count cur]} val]
              (let [nextCount (if (> val cur) (inc count) count)]
                {:cur val :count nextCount}))
            {:count 0 :cur Integer/MAX_VALUE} input))

(def part2 (reduce-kv
            (fn [{:keys [count cur]} index _]
              (let [current (apply + (subvec (into input [0 0]) index (+ 3 index)))
                    nextCount (if (> current cur) (inc count) count)]
                {:cur current :count nextCount}))
            {:count 0 :cur Integer/MAX_VALUE} input))
