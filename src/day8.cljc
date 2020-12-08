(ns day8)

(def input (-> "./input/day8input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(defmulti process-instruction (fn [instruction _] (first instruction))) ; [operation state]
(defmethod process-instruction :nop [_ [acc pointer]] [acc (inc pointer)])
(defmethod process-instruction :acc [[_ value] [acc pointer]] [(+ acc value) (inc pointer)])
(defmethod process-instruction :jmp [[_ value] [acc pointer]] [acc (+ pointer value)])

(defn swap-instruction [[instruction _]] (if (= :nop instruction) [:jmp _] [:nop _]))
(defn swappable? [[instruction _]] (or (= :nop instruction) (= :jmp instruction)))

(loop [[acc pointer] [0 0] stack []]
  (let [[next-acc next-pointer] (process-instruction (nth input pointer) [acc pointer])]
    (if (some? (some #(= next-pointer %) stack))
      acc
      (recur [next-acc next-pointer] (conj stack pointer))))) ; Part 1

(defn change-code-instruction [code skip]
  (let [index (->> code
                   (map-indexed vector)
                   (filter #(swappable? (second %)))
                   (drop skip)
                   (ffirst))]
    (update code index swap-instruction)))

(loop [[acc pointer] [0 0] stack [] code input changes 0]
  (if (= (count input) pointer)
    acc
    (let [[next-acc next-pointer] (process-instruction (nth code pointer) [acc pointer])]
      (if (some? (some #(= next-pointer %) stack))
        (recur [0 0] [] (change-code-instruction input changes) (inc changes))
        (recur [next-acc next-pointer] (conj stack pointer) code changes))))) ; Part 2
