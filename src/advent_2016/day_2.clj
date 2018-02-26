(ns advent-2016.day-2
  (:require [clojure.string :as str]))

(defn position->num [location]
  (cond
    (= location {:x -1 :y 1}) 1
    (= location {:x 0 :y 1}) 2
    (= location {:x 1 :y 1}) 3
    (= location {:x -1 :y 0}) 4
    (= location {:x 0 :y 0}) 5
    (= location {:x 1 :y 0}) 6
    (= location {:x -1 :y -1}) 7
    (= location {:x 0 :y -1}) 8
    (= location {:x 1 :y -1}) 9))

(defn position->char [location]
  (cond
    (= location {:x 0 :y 2}) 1
    (= location {:x -1 :y 1}) 2
    (= location {:x 0 :y 1}) 3
    (= location {:x 1 :y 1}) 4
    (= location {:x -2 :y 0}) 5
    (= location {:x -1 :y 0}) 6
    (= location {:x 0 :y 0}) 7
    (= location {:x 1 :y 0}) 8
    (= location {:x 2 :y 0}) 9
    (= location {:x -1 :y -1}) "A"
    (= location {:x 0 :y -1}) "B"
    (= location {:x 1 :y -1}) "C"
    (= location {:x 0 :y -2}) "D"))

(defn determine-num [starting-position instructions]
  (let [ending-position (reduce (fn [position
                                     {:keys [operation axis]}]
                                  (let [val-init (operation (axis position))
                                        val-final (cond
                                                    (> val-init 1) 1
                                                    (< val-init -1) -1
                                                    :else val-init)]
                                    (assoc
                                      position
                                      axis
                                      val-final)))
                                starting-position
                                instructions)]
    ending-position))

(defn determine-char [starting-position instructions]
  (let [ending-position (reduce (fn [position
                                     {:keys [operation axis]}]
                                  (let [new-position (assoc
                                                       position
                                                       axis
                                                       (operation (axis position)))
                                        abs-val (+ (Math/abs (:x new-position))
                                                   (Math/abs (:y new-position)))
                                        final-position (cond
                                                         (> abs-val 2) position
                                                         (<= abs-val 2) new-position)]
                                    final-position))
                                starting-position
                                instructions)]
    ending-position))

(defn xform-instructions [instruction]
  (let [instruction-map {"R" {:axis :x
                              :operation inc}
                         "L" {:axis :x
                              :operation dec}
                         "U" {:axis :y
                              :operation inc}
                         "D" {:axis :y
                              :operation dec}}]
    (get instruction-map instruction)))

(defn parse-instructions [input-path]
  (map #(str/split %1 #"")
       (str/split (slurp input-path) #"\n")))

(defn case-1
  [input-path]
  (apply str
         (rest
           (map
             position->num
             (reductions
               determine-num
               {:x 0 :y 0}
               (map #(map xform-instructions
                          %1)
                    (parse-instructions input-path)))))))

(defn case-2
  [input-path]
  (apply str
         (rest
           (map
             position->char
             (reductions
               determine-char
               {:x -2 :y 0}
               (map #(map xform-instructions
                          %1)
                    (parse-instructions input-path)))))))
