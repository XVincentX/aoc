(ns day5 (:require [clojure.math.combinatorics :refer [cartesian-product]]
                   [clojure.set :refer [difference]]))

(def input (-> "./input/day5input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(defn decode-location-data [location-data seat-limits marker]
  (first (reduce (fn [acc val]
                   (let [st (int (/ (reduce + acc) 2))]
                     (if (= val marker)
                       (assoc acc 1 st)
                       (assoc acc 0 (inc st)))))
                 seat-limits location-data)))

(defn seat-id [row column] (+ (* 8 row) column))

(defn decode-seat-data [acc seat-data]
  (let [row (decode-location-data (take 7 seat-data) [0 127] \F)
        column (decode-location-data (drop 7 seat-data) [0 7] \L)]
    (assoc acc (seat-id row column) [row column])))

(def seat-map (reduce decode-seat-data {} input))

(apply max (keys seat-map)) ; Part 1

(def all-seats (cartesian-product (range 0 128) (range 0 8)))

(filter
 #(and (some? (get seat-map (dec %))) (some? (get seat-map (inc %))))
 (difference
  (set (map (fn [[row column]] (seat-id row column)) all-seats))
  (set (keys seat-map)))) ; Part 2
