(ns day1 (:require [clojure.math.combinatorics :as combo]))

(def input (-> "./input/day1input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(def cartesian-2 (combo/cartesian-product input input))
(def cartesian-3 (combo/cartesian-product input input input))

(defn adds-to-2020 [v] (= 2020 (apply + v)))

(apply * (first (filter adds-to-2020 cartesian-2)))
(apply * (first (filter adds-to-2020 cartesian-3)))
