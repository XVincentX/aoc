(ns day2 
  (:require
   [clojure.string :as string]))

(def input (-> "input/2.txt"
               slurp
               string/split-lines))

(def reports (map (fn [t] (map Integer/parseInt t)) (map (fn [t] (re-seq #"\d+" t)) input)))

(defn isValid? [report]
  (let [pairs (partition 2 1 report)
        inc? (> (- (second report) (first report)) 0)
        valid-pairs (filter (fn [[a b]]
                              (let [diff (abs (- a b))] 
                                (and
                                 (if (true? inc?) (> b a) (< b a))
                                 (not= 0 diff)
                                 (<= diff 3)
                                 (>= diff 1)))) pairs)]
    (= (count valid-pairs) (count pairs))))

(defn isFixable? [report]
  (if (isValid? report) false
      (let [alternatives
            (map-indexed
             (fn [idx _]
               (concat
                (subvec (vec report) 0 idx)
                (subvec (vec report) (inc idx)))) report)
            valid-alternatives (map isValid? alternatives)]
        
        (> (count (filter true? valid-alternatives)) 0))))

(def sol1 (->> reports
               (filter isValid?) 
               (count)))

(def sol2 (+ sol1 (->> reports
                       (filter isFixable?)
                       (count))))
