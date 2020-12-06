(ns day6 (:require [clojure.string :refer [split split-lines]]
                   [clojure.set :refer [intersection]]))

(def input (-> "./day6input.txt"
               (clojure.core/slurp)
               (split #"\n\n")))

(def any-answers (->> input
                      (map #(re-seq #"\w" %))
                      (map set)
                      (reduce #(+ %1 (count %2)) 0))) ; Part 1

(def all-answers (->> input
                      (map split-lines)
                      (map #(map (fn [x] (set (clojure.core/char-array x))) %))
                      (map #(apply intersection %))
                      (reduce #(+ %1 (count %2)) 0))) ; Part 2
