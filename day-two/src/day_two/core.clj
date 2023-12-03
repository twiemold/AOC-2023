(ns day-two.core)


;; this could be simplified ...
(defn create-counts [data]
  {"game-id" (first (map parse-long (filter parse-long (flatten (re-seq #"(\d+)\:" data)))))
   "green" (reduce max (map parse-long (filter parse-long (flatten (re-seq #"(\d+)\s\bgreen\b" data)))))
   "blue" (reduce max (map parse-long (filter parse-long (flatten (re-seq #"(\d+)\s\bblue\b" data)))))
   "red" (reduce max (map parse-long (filter parse-long (flatten (re-seq #"(\d+)\s\bred\b" data)))))})

;; part one
(defn check-counts [color-map]
  (if (or (> (get color-map "red") 12)
           (> (get color-map "green") 13)
           (> (get color-map "blue") 14))
    0
    (get color-map "game-id")))

;; part two
(defn get-cube-product [color-map]
  (* (get color-map "red")
     (get color-map "green")
     (get color-map "blue")))


(defn check-games* []
  (->> (line-seq (clojure.java.io/reader "input.txt"))
       (map #(create-counts %))
       (map #(get-cube-product %))
       (reduce +)))

(defn -main
  "I don't do a whole lot."
  []
  (println (check-games*)))
