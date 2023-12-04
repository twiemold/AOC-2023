(ns day-three.core
  (:require [clojure.set :as s]))


(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn create-number-object [num-map row-num]
  (let [x (get num-map 0)
        y row-num]
    {:value (parse-long (get num-map 1))
     :location [x y]
     :adjacent-vals (set (for [i (range (- x 1) (+ (count (get num-map 1)) x 1))
                               j (range (- y 1) (+ y 2))]
                           [i j]))}))

(defn get-numbers [input row-num]
  (let [nums-map (re-pos #"\d+" input)]
    (map #(create-number-object % row-num) nums-map)))

(defn create-symbol-object [sym-map row-num]
  ;; repeat symbols need to be accounted for
  (let [x (nth sym-map 0)
        y row-num]
    {:value (nth sym-map 1)
     :location [x y]}))

(defn get-symbols [input row-num]
  (let [symbols-map (re-pos #"[^\.\d]" input)]
    (map #(create-symbol-object % row-num) symbols-map)))

(defn parse-values [input row-num]
  [(get-symbols input row-num) (get-numbers input row-num)])

(defn is-part-number [nums-symbols]
  (let [symbol-loc-set (into #{} (map #(get % :location) (filter #(not (integer? (get % :value))) nums-symbols)))
        nums (filter #(integer? (get % :value)) nums-symbols)]
    (filter #(> (count (s/intersection (get % :adjacent-vals) symbol-loc-set)) 0) nums)))

(defn count-engine-parts* []
  (->> (line-seq (clojure.java.io/reader "input.txt"))
       ;; fix this hardcoding (transducer?)
       ;; (map #(get-num-indexes %2 %1) (range 140))
       (map #(parse-values %2 %1) (range 140))
       (flatten)
       (is-part-number)
       (map #(get % :value))
       (reduce +)
       ))

(defn -main
  "I don't do a whole lot."
  []
  (println (count-engine-parts*)))
