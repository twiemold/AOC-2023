(ns day-four.core
  (:require [clojure.set :as s]))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn parse-nums [str]
  (re-seq #"\d+" str))

(defn create-sets [[_ winning-nums-str my-nums-str]]
  [(into #{} (parse-nums winning-nums-str)) (into #{} (parse-nums my-nums-str))])

(defn count-winners [[winning-set my-set]]
  (let [matches (count (s/intersection winning-set my-set))]
    (if (> matches 0)
     (exp 2 (- matches 1))
     0))
  )

(defn count-winnings* []
  (->> (slurp "input.txt")
       (re-seq #"\bCard\b\s+\d+\:\s+((?>\d+\s+)+)\|\s+((?>\d+(?>\s+|$))+)")
       (map #(create-sets %))
       (map #(count-winners %))
       (reduce +)))

(defn -main
  "I don't do a whole lot."
  []
  (println (count-winnings*)))
