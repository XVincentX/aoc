(ns day5
  (:require [clojure.string :as str]))

(def input (slurp "src/input/5.txt"))

(def pages-order (->> (re-seq #"(\d+)\|(\d+)" input)
                      (map (fn [[_ a b]] [(parse-long a) (parse-long b)]))))

(def updates (->> input
                  (str/split-lines)
                  (drop (inc (count pages-order)))
                  (map #(str/split % #","))
                  (map #(map (fn [a] (parse-long a)) %))))

(defn is-valid-line [line [first second]]
  (let [pos-first (count (take-while #(not= % first) line))
        pos-second (count (take-while #(not= % second) line))
        line-count (count line)]
    (or
     (= pos-first line-count)
     (= pos-second line-count)
     (>= pos-second pos-first))))

(defn is-valid [line] (map #(is-valid-line line %) pages-order))

(defn sol1 [updates]
  (->> (for [update updates
             :let [lines-validation (is-valid update)
                   middle (nth update (quot (count update) 2))]
             :when (every? true? lines-validation)]
         middle)
       (apply +)))

(println (sol1 updates))

(defn sol2 []
  (let [invalid-lines
        (for [update updates
              :let [lines-validation (map #(is-valid-line update %) pages-order)]
              :when (some false? lines-validation)]
          update)
        fixed-lines (map
                     (fn [line]
                       (sort
                        (fn [a b]
                          (let [rule (first (filter #(or (= [b a] %) (= [a b] %)) pages-order))
                                [c1] rule]
                            (if (nil? rule) 0 (if (= c1 a) -1 1)))) line)) invalid-lines)
        middles (map #(nth % (quot (count %) 2)) fixed-lines)] 
    (apply + middles)))

(println (sol2))

