(ns day13
  (:require [clojure.string :as str]))

(defn parse-machine [text]
  (let [lines (str/split-lines text)
        pair #(let [[_ x y] (re-matches #".*: X.(\d+), Y.(\d+)" %)]
                [(parse-long x) (parse-long y)])]
    (mapv pair lines)))

(defn parse [text]
  (map parse-machine (str/split text #"\n\n")))

(def input (parse (slurp "src/input/13.txt")))

(defn sum [ns]
  (reduce + ns))

(def button-pairs
  (sort-by sum
           (for [i (range 0 101)
                 j (range 0 101)]
             [i j])))

(defn press [[dx1 dy1]
             [dx2 dy2]
             n1
             n2]
  [(+ (* n1 dx1) (* n2 dx2))
   (+ (* n1 dy1) (* n2 dy2))])

(defn play [[a b goal]]
  (for [[na nb] button-pairs
        :let [dest (press a b na nb)]
        :when (= dest goal)]
    [na nb]))

(defn score [[a b]]
  (+ b (* 3 a)))


(defn part1 [input]
  (sum
   (for [machine input
         :let [[presses] (play machine)]
         :when presses]
     (score presses))))

;; ----------------------------------------

(defn solve [[a1 a2] [b1 b2] [c1 c2]]
  (let [x (/ (- (* b2 c1) (* b1 c2))
             (- (* a1 b2) (* a2 b1)))
        y (/ (- (* c2 a1) (* c1 a2))
             (- (* a1 b2) (* a2 b1)))]
    [x y]))

(defn valid? [solution]
  (every? #(= % (long %)) solution))

(defn part2 [input]
  (sum
   (for [[a b goal] input
         :let [presses (solve a b (map #(+ 10000000000000 %) goal))]
         :when (valid? presses)]
     (score presses))))

(part1 input)
(part2 input)
