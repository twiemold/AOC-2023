(ns day-four.core
  (:require [clojure.set :as s])
  (:gen-class))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn parse-nums [my-str]
  (map parse-long (re-seq #"\d+" my-str)))

(defn create-sets [data]
  (into {} (map #(vector (parse-long (nth % 1))
                         [(into #{} (parse-nums (nth % 2)))
                          (into #{} (parse-nums (nth % 3)))])
                data))
  )

;; part one
(defn count-winners [[winning-set my-set]]
  (let [matches (count (s/intersection winning-set my-set))]
    (if (> matches 0)
     (exp 2 (- matches 1))
     0))
  )

(defn count-winners-v2
  [master-card-map to-be-processed counter]
  (if (empty? to-be-processed)
    counter
    (let [card (first to-be-processed)
          card-num (get card 0)
          sets (get card 1)
          cache-hit (get (get master-card-map card-num) 3)
          matches (if cache-hit
                    cache-hit
                    (count (s/intersection (get sets 0) (get sets 1))))
          new-cards (mapv #(vector % (get master-card-map %))
                         (range (+ card-num 1) (+ card-num 1 matches)))]
      (if (> matches 0)
        #(count-winners-v2 (if cache-hit master-card-map (assoc master-card-map card-num (conj (get master-card-map card-num) matches)))
                           (into (rest to-be-processed) new-cards)
                           (inc counter))
        #(count-winners-v2 (if cache-hit master-card-map (assoc master-card-map card-num (conj (get master-card-map card-num) matches)))
                           (rest to-be-processed)
                           (inc counter))
        )
      )
    )
  )


(defn count-winnings* []
  (->> (slurp "input.txt")
       (re-seq #"\bCard\b\s+(\d+)\:\s+((?>\d+\s+)+)\|\s+((?>\d+(?>\s+|$))+)")
       (create-sets)
       (trampoline #(count-winners-v2 % (vec %) 0 ))))

(defn -main
  "I don't do a whole lot."
  []
  (println (count-winnings*)))
