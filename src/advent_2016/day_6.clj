(ns advent-2016.day-6
  (:require [clojure.string :as str]))

(defn case-1
  [input-path]
  (apply str (map (fn [m]
                    (first (last (sort-by val m))))
                  (map frequencies
                       (first (let [strs (map vec
                                              (str/split (slurp input-path)
                                                         #"\n"))]
                                (map (partial apply map vector)
                                     (partition (count strs) strs))))))))

(defn case-2
  [input-path]
  (apply str (map (fn [m]
                    (first (first (sort-by val m))))
                  (map frequencies
                       (first (let [strs (map vec
                                              (str/split (slurp input-path)
                                                         #"\n"))]
                                (map (partial apply map vector)
                                     (partition (count strs) strs))))))))
