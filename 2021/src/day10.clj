(ns day10 (:require [clojure.string :refer [split-lines]]))

(def input (-> "./input/day10input.txt"
               clojure.core/slurp
               split-lines))

(def rev {\] \[
          \) \(
          \} \{
          \> \<})

(def points {\( 3
             \[ 57
             \{ 1197
             \< 25137})          

(defn first-corrupted-char [n]
  (reduce
   (fn [state curChar]
     (if (get state curChar)
       (-> state
           (update curChar inc)
           (update :next conj curChar))
       (let [r (rev curChar)
             next (-> state :next first)]
         (if (not= next r)
           (reduced r)
           (-> state
               (update r dec)
               (update :next rest))))))
   {\[ 0 \( 0 \{ 0 \< 0} n))   

(def part1
  (reduce
   (fn [state line]
     (let [p (first-corrupted-char line)]
       (+ state (get points p 0)))) 0 input))

(def points-2 {\( 1
               \[ 2
               \{ 3
               \< 4})

(def incomplete-points
  (->>
   (reduce
    (fn [state line]
      (let [p (first-corrupted-char line)]
        (if (number? p) state
            (conj state (reduce
                         (fn [s t]
                           (-> s
                               (* 5)
                               (+ (get points-2 t))))
                         0 (:next p)))))) [] input)
   (filter (complement zero?))
   sort))

(def part2 (nth incomplete-points (quot (count incomplete-points) 2)))
