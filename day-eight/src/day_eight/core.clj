(ns day-eight.core
  (require '[clojure.string :as str]))

(defn process-commands [instructions network target-node start-node]
  (loop [steps 0
         instrs (cycle instructions)
         curr-node start-node]
    (let [neighbors (get network curr-node)] 
      (case (first instrs)
        ;; if the matched node from the map is target-node, return (inc steps) else recur on matched node
        \L (if (= (first neighbors) target-node)
             (inc steps)
             (recur (inc steps) (rest instrs) (first neighbors)))
        \R (if (= (second neighbors) target-node)
             (inc steps)
             (recur (inc steps) (rest instrs) (second neighbors)))))))

(defn create-map [input]
  (apply merge
         (map #(hash-map (first %) (drop 1 %))
              (map #(drop 1 %)
                   (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %)
                        (drop 2 input))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input (line-seq (clojure.java.io/reader "input.txt"))
        network (create-map input)
        instructions (first input)]
    (process-commands instructions network "ZZZ" "AAA")
    ))

(comment 
  (:a {:a 1 :b 2})
  (first (rest (rest (rest (cycle "ABC")))))
  (->Node "AAA" "BBB" "CCC")
  (node-from-seq '("AAA" "BBB" "CCC"))
  (defn my-test-func [my-seq] 
      (hash-map (first my-seq) (drop 1 my-seq)))
  
  (apply merge (map my-test-func (map #(drop 1 %) (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %) (drop 2 (line-seq (clojure.java.io/reader "input.txt"))))) ) )
  (map my-test-func (map #(drop 1 %) (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %) (drop 2 (line-seq (clojure.java.io/reader "input.txt"))))) )
  
  (map node-from-seq (map #(drop 1 %) (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %) (drop 2 (line-seq (clojure.java.io/reader "input.txt"))))))
  (map #(drop 1 %) (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %) (drop 2 (line-seq (clojure.java.io/reader "input.txt")) ) ) )
  (map #(re-find #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" %) (drop 2 (line-seq (clojure.java.io/reader "input.txt")) ) )
  (drop 2 (line-seq (clojure.java.io/reader "input.txt")))
  (drop 2 (str/split (slurp "input.txt") #"\n") )
  (subs "AAAB" 0 3)
  (apply rest (re-seq #"(\D{3})\s\=\s\((\D{3})\,\s(\D{3})\)" "PBN = (JRP, RVT)") )
  (process-commands "LRLRLR")
  
  (take 1 (str/split (slurp "input.txt") #"\n"))
  (-main) 
  )

