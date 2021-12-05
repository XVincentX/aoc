(ns day4 (:require [clojure.string :refer [split-lines split blank?]]))

(defn positions
  [pred coll]

  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(def input (->> "./input/day4input.txt"
                clojure.core/slurp
                split-lines))

(def numbers-will-draw (-> input
                           first
                           (split #",")
                           (as-> t (mapv clojure.core/parse-long t))))

(def marked-boards (->> input
                        rest
                        (filter (complement blank?))
                        (mapcat (fn [line]
                                  (->>
                                   (split line #"\s+")
                                   (filter (complement blank?))
                                   (mapv (fn [value] {:m? false :number (clojure.core/parse-long value)})))))
                        (into [])))

(def boards (->> input
                 rest
                 (filter (complement blank?))
                 (mapcat (fn [line]
                           (->>
                            (split line #"\s+")
                            (filter (complement blank?))
                            (mapv clojure.core/parse-long))))

                 (partition 5)
                 (partition 5)
                 (into [])))



(defn line-complete? [line] (every? :m? line))

(defn find-line-complete-index [boards]
  (->> boards
       (partition 5)
       (positions line-complete?)
       first))

(defn compute-winning-boards [marked-boards numbers-to-draw]
  (let [[_ _ row-indexes drawn-numbers-history]
        (reduce (fn [[board-state drawn-numbers winning-line-indexes drawn-numbers-history] drawn-number]
                  (let [drawn-number-positions (positions (fn [t] (= (:number t) drawn-number)) marked-boards)
                        next-state (reduce (fn [st number-position]
                                             (update st number-position assoc :m? true))
                                           board-state drawn-number-positions)
                        next-drawn-numbers (conj drawn-numbers drawn-number)]

                    (if-let [complete-line-index (find-line-complete-index next-state)]
                      [next-state next-drawn-numbers (conj winning-line-indexes complete-line-index) (conj drawn-numbers-history next-drawn-numbers)]
                      [next-state next-drawn-numbers winning-line-indexes drawn-numbers-history])))
                [marked-boards [] [] []] numbers-to-draw)]
    [row-indexes drawn-numbers-history]))

(def part1
  (let [[row-indexes drawn-numbers-history] (compute-winning-boards marked-boards numbers-will-draw)
        row-index (first row-indexes)
        drawn-numbers (first drawn-numbers-history)
        matrix-index (quot row-index 5)
        matrix (nth boards matrix-index)
        matrix-sum (apply + (clojure.set/difference (set (mapcat identity matrix)) (set drawn-numbers)))]

    (* matrix-sum (last drawn-numbers))))

(def part2
  (let [[row-indexes drawn-numbers-history] (compute-winning-boards marked-boards numbers-will-draw)
        row-index (last row-indexes)
        drawn-numbers (last drawn-numbers-history)
        matrix-index (quot row-index 5)
        matrix (nth boards matrix-index)
        matrix-sum (apply + (clojure.set/difference (set (mapcat identity matrix)) (set drawn-numbers)))]

    (* matrix-sum (last drawn-numbers))))
