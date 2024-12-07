(ns day7 (:require [clojure.string :as str]
                   [clojure.math.combinatorics :as combo]))

(def input (str/split-lines (slurp "src/input/7.txt")))

(defn line-parse [line]
  (->> line
       (re-seq #"\d+")
       (map parse-long)))

(defn op-exec [op a b]
  (if (= op "||") (parse-long (str b a)) (op a b)))

(defn combine-numbers [operations numbers start]
  (if (or (empty? numbers) (empty? operations))
    start
    (recur (rest operations) (rest numbers) (op-exec (first operations) (first numbers) start))))

(defn can-sum-or-multiply? [op-set [final & numbers]]
  (let [operations-count (-> numbers count dec)
        operations-combinations (combo/selections op-set operations-count)
        results-combinations (map #(combine-numbers % (rest numbers) (first numbers)) operations-combinations)]

    (some #(= final %) results-combinations)))

(def parsed-input (map line-parse input))    

(def sol1 (->> parsed-input
               (filter (partial can-sum-or-multiply? [+ *]))
               (map first)
               (apply +)))

(def sol2 (->> parsed-input
               (filter (partial can-sum-or-multiply? [+ * "||"]))
               (map first)
               (apply +)))
