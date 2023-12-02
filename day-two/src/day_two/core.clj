(ns day-two.core)


;; this could be simplified ...
(defn create-counts [data]
  {"game-id" (reduce + (map parse-long (filter parse-long (flatten (filter some? (map #(re-find #"(\d+)\:" %) data))))))
   "green" (reduce max (map parse-long (filter parse-long (flatten (filter some? (map #(re-find #"(\d+)\s\bgreen\b" %) data))))))
   "blue" (reduce max (map parse-long (filter parse-long (flatten (filter some? (map #(re-find #"(\d+)\s\bblue\b" %) data))))))
   "red" (reduce max (map parse-long (filter parse-long (flatten (filter some? (map #(re-find #"(\d+)\s\bred\b" %) data))))))})

(defn check-counts [color-map]
  (if (or (> (get color-map "red") 12)
           (> (get color-map "green") 13)
           (> (get color-map "blue") 14))
    0
    (get color-map "game-id")))


(defn check-games* []
  (->> (line-seq (clojure.java.io/reader "input.txt"))
       (map #(re-seq #"\d+\:|\d+\s\w+|\d+\s\w+" %))
       (map #(create-counts %))
       (map #(check-counts %))
       (reduce +)
       (println)))

(defn -main
  "I don't do a whole lot."
  []
  (check-games*))
