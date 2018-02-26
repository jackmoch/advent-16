(ns advent-2016.day-1
  (:require [clojure.string :as str]
            [clojure.string :as str]))

(defn walk [{:keys [facing
                    x-cord
                    y-cord] :as location}
            distance]
  (let [dir->walk-fn {:north (fn [coll
                                  distance]
                               (assoc
                                 coll
                                 :x-cord (+ x-cord
                                            distance)))
                      :east (fn [coll
                                 distance]
                              (assoc
                                coll
                                :y-cord (+ y-cord
                                           distance)))
                      :south (fn [coll
                                  distance]
                               (assoc
                                 coll
                                 :x-cord (- x-cord
                                            distance)))
                      :west (fn [coll
                                 distance]
                              (assoc
                                coll
                                :y-cord (- y-cord
                                           distance)))}
        walk-fn (facing dir->walk-fn)]
    (walk-fn location
             distance)))

(defn turn
  ([current-position instruction]
   (let [turn-constructor (fn [turn-fn]
                            (assoc
                              current-position
                              :facing
                              (turn-fn (:facing current-position))))
         turn-left (fn [k]
                     (k {:north :west
                         :west :south
                         :south :east
                         :east :north}))
         turn-right (fn [k]
                      (k {:north :east
                          :east :south
                          :south :west
                          :west :north}))]
     (case instruction
       "L" (turn-constructor turn-left)
       "R" (turn-constructor turn-right)))))

(defn walk [{:keys [facing
                    x-cord
                    y-cord] :as location}
            distance]
  (let [walk-fn-constructor (fn [cord op]
                              (fn [coll distance]
                                (assoc
                                  coll
                                  cord (op (cord coll)
                                           distance))))
        dir->walk-fn {:north (walk-fn-constructor :x-cord
                                                  +)
                      :south (walk-fn-constructor :x-cord
                                                  -)
                      :east (walk-fn-constructor :y-cord
                                                 +)
                      :west (walk-fn-constructor :y-cord
                                                 -)}
        walk-fn (facing dir->walk-fn)]
    (walk-fn location
             distance)))

(defn parse-instruction [instruction]
  (let [turn-instruction (first (str/split instruction #""))
        distance (read-string (str/join (rest (str/split instruction #""))))]
    {:turn-instruction turn-instruction
     :distance distance}))

(defn task-one [instruction-list]
  (let [starting-position {:x-cord 0
                           :y-cord 0
                           :facing :north}
        final-position (reduce (fn [{:keys [facing] :as current-position}
                                    {:keys [turn-instruction
                                            distance]}]
                                 (walk (turn current-position
                                             turn-instruction)
                                       distance))
                               starting-position
                               instruction-list)]
    (+ (Math/abs (:x-cord final-position))
       (Math/abs (:y-cord final-position)))))

(defn -main [input-path]
  (let [instructions (map parse-instruction
                          (str/split (slurp input-path) #", "))]
    (task-one instructions)))
