(ns day-one.core)

(defn grab-digits [[_ x y z]]
  (if x (parse-long (str x y)) (parse-long (str z z))))

(defn sum-calibration-vals* []
  (->> (slurp "input.txt")
       (re-seq #"(?m)^\D*(\d{1})\w*(\d{1})(?!.*\d)|\w*(\d{1})")
       (map #(grab-digits %))
       (reduce +)))

(defn -main
  "I don't do a whole lot."
  []
  (println (sum-calibration-vals*)))
