(ns day2)

(def input (-> "./input/day2input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(->> input
     (filter (fn [[min max letter pwd]]
               (let [occurrences (count (filter #(= letter %) pwd))]
                 (and (>= occurrences min) (<= occurrences max)))))
     count)

(->> input
     (filter (fn [[min max letter pwd]]
               (let [l1 (nth pwd (dec min)) l2 (nth pwd (dec max))]
                 (and (or (= letter l1) (= letter l2)  (= letter l2))
                      (not (= l2 l1))))))
     count)
