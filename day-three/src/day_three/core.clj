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

(defn create-symbol-object [sym-map row-num]
  (let [x (nth sym-map 0)
        y row-num]
    {:value (nth sym-map 1)
     :location [x y]}))

(defn parse-values [row-num input]
  [(map #(create-symbol-object % row-num) (re-pos #"[^\.\d]" input))
   (map #(create-number-object % row-num) (re-pos #"\d+" input))])

;; part one
(defn is-part-number [nums-symbols]
  (let [symbol-loc-set (into #{} (map #(get % :location) (filter #(not (integer? (get % :value))) nums-symbols)))
        nums (filter #(integer? (get % :value)) nums-symbols)]
    (filter #(> (count (s/intersection (get % :adjacent-vals) symbol-loc-set)) 0) nums)))

;; the actual symbol-number association details are a tad
;; hackish and also assume that a number is associated with
;; no more than one symbol, which happens to be true
;; for this input
(defn map-syms-to-nums [nums-symbols]
  (let [symbols (filter #(not (integer? (get % :value))) nums-symbols)
        nums (filter #(integer? (get % :value)) nums-symbols)
        symbol-by-loc (into {} (map #(vector (get % :location) %) symbols))
        symbol-loc-set (into #{} (keys symbol-by-loc))]
    (for [num nums
          :let [adjacent-sym (s/intersection (get num :adjacent-vals) symbol-loc-set)
                new-num (assoc num :adjacent-syms (first (map #(get symbol-by-loc %) (map vec adjacent-sym))))]
          :when (> (count adjacent-sym) 0)]
      new-num)
    ))


(defn count-engine-parts* []
  (->> (line-seq (clojure.java.io/reader "input.txt"))
       ;; fix this hardcoding (transducer?)
       (map #(parse-values % %2) (range 140))
       (flatten)
       (map-syms-to-nums)
       (group-by #(get % :adjacent-syms))
       (filter #(and (= (get (first %) :value) "*") (= (count (second %)) 2)))
       (flatten)
       (filter #(integer? (get % :value)))
       (map #(get % :value))
       (partition 2)
       (map #(apply * %))
       (reduce +)))

(defn -main
  "I don't do a whole lot."
  []
  (println (count-engine-parts*)))
