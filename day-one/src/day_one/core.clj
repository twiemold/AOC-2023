(ns day-one.core)

(def nums-map {"one" "1"
               "two" "2"
               "three" "3"
               "four" "4"
               "five" "5"
               "six" "6"
               "seven" "7"
               "eight" "8"
               "nine" "9"
               "1" "1"
               "2" "2"
               "3" "3"
               "4" "4"
               "5" "5"
               "6" "6"
               "7" "7"
               "8" "8"
               "9" "9"})

(defn grab-digits [[_ x y z]]
  "Discard match val and parse digits"
  (if x (map #(get nums-map %) [x y]) (seq [z z])))

;; part one
(defn sum-calibration-vals* []
  (->> (slurp "input.txt")
       (re-seq #"(?m)^\D*(\d{1})\w*(\d{1})(?!.*\d)|\w*(\d{1})")
       (map #(grab-digits %))
       (reduce +)))

(defn sum-calibration-vals-v2* []
  (->> (slurp "input.txt")
       (re-seq #"(?m)((?>one|two|three|four|five|six|seven|eight|nine)|(?>\d{1})).*((?>one|two|three|four|five|six|seven|eight|nine)(?!.*(?>one|two|three|four|five|six|seven|eight|nine))(?!.*\d)|(?>\d{1})(?!.*\d)(?!.*(?>one|two|three|four|five|six|seven|eight|nine)))|(\d{1})")
       (map #(grab-digits %))
       (map #(reduce str %))
       (map parse-long)
       (reduce +)))

(defn -main
  "I don't do a whole lot."
  []
  (println (sum-calibration-vals-v2*)))
