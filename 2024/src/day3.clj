(ns day3)

(def input (-> "input/3.txt" slurp))

(def sol1 (->> input
               (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
               (map (fn [[_ a b]] (* (Integer/parseInt a) (Integer/parseInt b))))
               (apply +)))

(def sol2 (->> input
               (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")
               (reduce (fn [[state sum] [elem a b]]
                         (cond
                           (= "do()" elem) [true sum]
                           (= "don't()" elem) [false sum]
                           (true? state) [state (+ sum (* (Integer/parseInt a) (Integer/parseInt b)))]
                           :else [state sum])) [true 0])))
