(ns day10)

(def input (-> "./input/day10input.txt"
               clojure.core/slurp
               clojure.core/read-string))

(def input-with-device (conj input (+ 3 (apply max input))))

(defn find-adapters-for [adapters-set joltage]
  (filter #(and (>= % joltage) (>= joltage (- % 3))) (disj adapters-set joltage)))

(defn get-lowest-adapter-for [adapters-set joltage]
  (->> joltage
       (find-adapters-for adapters-set)
       sort
       first))

(loop [adapters input-with-device differences-map {} target-joltage 0]
  (if (zero? (count adapters))
    differences-map
    (let [selected-adapter (get-lowest-adapter-for adapters target-joltage)
          difference (Math/abs (- target-joltage selected-adapter))]
      (recur (disj adapters selected-adapter) (update differences-map (keyword (.toString difference)) #(if (nil? %) 1 (inc %))) selected-adapter))))

(reduce (fn [acc val]
          (let [possible-adapters (find-adapters-for input val)]
            (if (> (count possible-adapters) 1)
              (* acc (count possible-adapters))
              acc))) 1 (conj input-with-device 0))
