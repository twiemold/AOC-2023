(ns day-seven.core
  (:require [clojure.string :as str]))

(defrecord Hand [value cards bet])

(def card->value
  {\2 0
   \3 1
   \4 2
   \5 3
   \6 4
   \7 5
   \8 6
   \9 7
   \T 8
   \J 9
   \Q 10
   \K 11
   \A 12})

(defn sort-hands [hand-one hand-two]
  (compare (into [] (concat (vector (:value hand-one))
                            (mapv card->value (:cards hand-one)))) 
           (into [] (concat (vector (:value hand-two))
                            (mapv card->value (:cards hand-two))))))

(defn classify-hands [ [cards bet] ]
  (let [sorted-freq (sort > (map second (frequencies cards) ) )
        int-bet (parse-long bet)]
   (case (first sorted-freq)
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
       (map * (drop 1 (range) ))
       (reduce +)
       ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print (calculate-winnings)))

(comment 
  ()
  (let [hand-one (Hand. 5 "KKKKK")
        hand-two (Hand. 5 "AAAAA")
        hand-three (Hand. 4 "AAAAK")
        hand-four (Hand. 4 "AAAAJ")
        hands [hand-one hand-two hand-three hand-four]]
    ;; (compare (:value hand-one) (:value hand-two))
    (sort sort-hands hands)
    ;; (map card->value (:cards hand-one)))
  )
