(ns day-seven.core
  (:require [clojure.string :as str]))

(defrecord Hand [value cards bet])

(def card->value
  (zipmap '(\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A) (range)))

(defn sort-hands [hand-one hand-two]
  (compare (into [] (concat (vector (:value hand-one))
                            (mapv card->value (:cards hand-one)))) 
           (into [] (concat (vector (:value hand-two))
                            (mapv card->value (:cards hand-two))))))

(defn classify-hands [[cards bet]]
  (let [freq (frequencies cards)
        freq-sans-j (dissoc freq \J)
        sorted-freq (sort > (vals freq-sans-j))
        joker-val (get freq \J 0)
        max-val (if (empty? freq-sans-j) 0 (first sorted-freq) )
        int-bet (parse-long bet)]
   (case (+ joker-val max-val)
      1 (Hand. 1 cards int-bet) ;; high card  
      2 (if (= (second sorted-freq) 1)
          (Hand. 2 cards int-bet) ;; one pair
          (Hand. 3 cards int-bet)) ;; two pair
      3 (if (= (second sorted-freq) 1)
          (Hand. 4 cards int-bet) ;; three of a kind
          (Hand. 5 cards int-bet)) ;; full house
      4 ( Hand. 6 cards int-bet )
      5 ( Hand. 7 cards int-bet))))

(defn calculate-winnings []
  (->> (line-seq (clojure.java.io/reader "input.txt"))
       (map #(str/split % #" "))
       (map classify-hands)
       (sort sort-hands)
       (map :bet)
       (map * (drop 1 (range)))
       (reduce +)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (calculate-winnings)))

