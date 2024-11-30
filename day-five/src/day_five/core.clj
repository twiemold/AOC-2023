(ns day-five.core
  (:require [clojure.string :as str]))

(defn get-dest-value [[source-range dest-range _ seed]]
  (let [diff (- seed dest-range)]
    (+ diff source-range)))

(defn lookup-seed [entries seedling]
  (loop [ranges entries
         seed seedling]
    ;; if in map, get-dest-value and return value
    ;; else loop to next line of map
    ;; if map is exhuasted, return the number itself
    (let [entry (first ranges)]
     (if
        entry
        ; map is unexhausted and needs to be checked. This holds recursive case
        (if (and (<= (nth entry 1) seed) (< seed (+ (nth entry 2) (nth entry 1))))
          (get-dest-value (conj entry seed)) 
          (recur (rest ranges) seed)) 
        ;; map is exhausted, default case of source number
        seed))))

(defn map-seed [almanac seedling]
  (loop [alm almanac
         seed seedling]
    (let [chart (first alm)]
     (if
        chart 
        (recur (rest alm) (lookup-seed chart seed))
        seed))))

(defn read-map [data loc]
  (map vec (partition 3 (drop 2 (map parse-long (str/split (nth data loc) #" |\n"))))))

(defn -main
  [& args]
  (let [test false
        data (str/split (slurp (if test "test-input.txt" "input.txt")) #"\n\n")
        seeds (drop 1 (map parse-long (str/split (nth data 0) #" ")))
        almanac (for [x (drop 1 (range (count data)))]
                  (read-map data x))]
    (apply min (for [seed seeds]  
        (map-seed almanac seed))))
  )

(comment
  (in-map? '([52 50 48]) 98)
  (get-dest-value [52 50 48 79])
  )
