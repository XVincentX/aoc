(ns day9)

(def input (slurp "src/input/9.txt"))

(defn expand-disk-format [number-as-string]
  (loop
   [num-seq (->> number-as-string
                 (seq)
                 (map Character/getNumericValue))
    even? true
    idx 0
    result []]
    (if (empty? num-seq)
      (reduce into [] result)
      (recur
       (rest num-seq)
       (not even?)
       (if even? (inc idx) idx)
       (conj result (repeat (first num-seq)
                            (if even? idx \.)))))))

(defn move-block [blocks]
  (let [reversed (reverse blocks)
        last-data-block (first (drop-while #(= \. %) reversed))
        first-free-block-index (.indexOf blocks \.)
        last-data-block-index (.lastIndexOf blocks last-data-block)]
    (if (= 1 (- last-data-block-index first-free-block-index)) blocks
        (recur (-> blocks
                   vec
                   (assoc first-free-block-index last-data-block)
                   (assoc last-data-block-index \.))))))


(defn checksum [blocks]
  (reduce
   (fn [[sum position] elem] [(if (= \. elem) sum (+ sum (* position elem))) (inc position)]) [0 0] blocks))

(defn replace-block [free files]
  (->> free (drop (count files)) (concat files)))

(defn move-file-to-block [blocks file]
  (let [target (first (drop-while #(or (not= \. (first %)) (< (count %) (count file))) blocks))
        target-index (.indexOf blocks target)
        new-block (replace-block target file)
        file-index (.lastIndexOf blocks file)]
    (if (or (= -1 target-index) (> target-index file-index))
      blocks
      (-> blocks
          (assoc target-index new-block)
          (assoc file-index (repeat (count file) \.))))))

(defn move-files [blocks file-blocks] 
  (reduce (fn [blocks file] (vec (partition-by identity (flatten (move-file-to-block blocks file))))) blocks file-blocks))

(comment (def sol1 (-> input expand-disk-format move-block checksum)))

(defn sol2 []
  (let [blocks (-> input expand-disk-format)
        b (->> blocks (partition-by identity) vec)
        file-blocks (->> b reverse (filter #(not= \. (first %))))]
    (checksum (flatten (move-files b file-blocks)))))
