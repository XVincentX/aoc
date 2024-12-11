(ns day11)

(def input [7725 185 2 132869 0 1840437 62 26310])

(defn divide-number [n]
  (->> n
       str
       (split-at (/ (count (str n)) 2))
       (map #(apply str %))
       (map parse-long)))

(defn next-step [line]
  (reduce (fn [new-line current]
            (cond
              (zero? current) new-line
              (even? (count (str current))) (inc new-line)
              :else new-line )) 0 line))

(def blink!
  (memoize
   (fn [stone ntimes]
     (cond
       (= ntimes 0)
       1

       (= stone 0)
       (blink! 1 (dec ntimes))

       (even? (count (str stone)))
       (let [text (str stone)
             n (count text)
             [l r] (mapv parse-long
                         (map #(apply str %)
                              (split-at (/ n 2) text)))]
         (+ (blink! l (dec ntimes))
            (blink! r (dec ntimes))))

       :else
       (blink! (* 2024 stone) (dec ntimes))))))

(def sol1 (nth (iterate next-step input) 25))
(def sol2 (->> input (map #(blink! % 75)) (apply +)) )

