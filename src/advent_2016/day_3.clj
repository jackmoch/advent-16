(ns advent-2016.day-3
  (:require [clojure.string :as str]))

(defn valid-triangle
  [coll]
  (let
    [sorted-coll (sort > coll)
     high-num (first sorted-coll)
     remaining-nums (rest sorted-coll)]
    (if (< high-num
           (apply + remaining-nums))
      true
      false)))

(defn return-valid-triangles
  [triangles]
  (filter valid-triangle
          triangles))

(defn parse [coll]
  (mapv #(Integer/parseInt %)
        (str/split (str/trim coll)
                   #"\s+")))

(defn -main
  [input-path]
  (count
    (return-valid-triangles
      (mapv parse
            (str/split (slurp input-path)
                       #"\n")))))
