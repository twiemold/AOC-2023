(ns day-six.core)


(defn zip-vectors [data]
  (let [[first-half second-half] (split-at (/ (count data) 2) data)]
    (map vector (map parse-long first-half) (map parse-long second-half))
    )
  )

(defn distance-formula [x time]
  (* x (- time x))
  )

(defn calc-distance [[time distance]]
  (filter #(> % distance) (map #(distance-formula % time) (range (+ time 1))))
  )


(defn calc-races* []
  (->> (slurp "input.txt")
       (re-seq #"\d+")
       (vec)
       (zip-vectors)
       (map calc-distance)
       (map count)
       (reduce *)
       ))

(defn -main
  "I don't do a whole lot."
  []
  (println (calc-races*)))
