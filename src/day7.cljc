(ns day7 (:require [clojure.string :refer [split-lines split]]))

(defn get-data-from-line [acc line]
  (let [target-bag (-> line
                       (split #" ")
                       (as-> s (take 2 s))
                       (as-> s (interpose " " s))
                       (as-> s (apply str s)))
        matches (re-seq #"(?:(\d+) (\S* \S*) bags?(?:,|.)\s?)" line)]
    (assoc acc target-bag (reduce #(assoc %1 (nth %2 2) (Integer. (second %2))) {} matches))))

(defn get-containing-bag-type [bag-type map]
  (filter (fn [[_ inner-map]] (get inner-map bag-type)) map))

(def bags-map (->> "./day7input.txt"
                   clojure.core/slurp
                   split-lines
                   (reduce get-data-from-line {})))

(defn bags-containing [bag-type]
  (let [containing-bags (get-containing-bag-type bag-type bags-map)]
    (into (set (map first containing-bags))
          (mapcat #(bags-containing (first %)) containing-bags))))

(count (bags-containing "shiny gold")) ; Part 1

(defn bags-count [bag-type]
  (let [bag-content (get bags-map bag-type)]
    (inc (apply + (map #(let [bag-mul (second %)
                              inner-count (max 1 (bags-count (first %)))]
                          (* bag-mul inner-count)) bag-content)))))

(dec (bags-count "shiny gold")) ; Part 2
