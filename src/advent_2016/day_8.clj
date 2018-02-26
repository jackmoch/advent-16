(ns advent-2016.day-8
  (:require [clojure.string :as str]
            [clojure.data :refer [diff]]))

;0. board
;1. parser to get instructions
;2. instructions
;3. apply instructions to board
;
;Instructions
;1. rect
;  - width
;  - height
;[2.] rotate column
;  - column
;  - distance
;[3.] rotate row
;  - row
;  - distance
;
;Given Instructions
;rect 3x2
;rotate column x=1 by 1
;rotate row y=0 by 4
;rotate column x=1 by 1

;(defn parse
;  [s]
;  (let [s (str/split s #" ")
;        rect (fn [[i p]]
;               (let [[w h] (map #(Integer/parseInt %)
;                                (str/split p #"x"))]
;                 {:width w
;                  :height h}))]
;    (case (count s)
;      2 (rect s)
;      5 nil)))
;
;(defn case-1
;  [input-path]
;  (reduce (fn [a {:keys [width height]}]
;            (+ a
;               (* width height)))
;          0
;          (remove nil?
;                  (map parse
;                       (str/split-lines (slurp input-path))))))
;(def permutations
;  {1 [1 0 0]
;   2 [1 1 0]
;   3 [1 1 1]})
;
;(defn overlay
;  [acoll bcoll]
;  (into []
;        (map (fn [a b]
;               (into []
;                     (map (fn [a b]
;                            (if (or (= 1 a)
;                                    (= 1 b))
;                              1
;                              0))
;                          a
;                          b)))
;             acoll
;             bcoll)))
;
;(reduce (fn [acc {:keys [height
;                         width]}]
;          (let [generated-board (into []
;                                      (concat (repeat width
;                                                      (permutations height))
;                                              (repeat (- 7
;                                                         width)
;                                                      [0 0 0])))]
;            (overlay board
;                     generated-board)))
;        board
;        [{:height 2
;          :width 3}])

(defn board-transpose [board]
  (apply mapv vector board))

(defn
  gen-board
  [columns rows]
  (into []
        (repeat columns
                (into []
                      (repeat rows 0)))))

(defn
  gen-board-positions
  [x y]
  (let [x-coll (repeat y
                       (into []
                             (range x)))
        y-coll (range y)]
    (mapcat (fn [xc y1]
              (map (fn [x1]
                     [x1 y1])
                   xc))
            x-coll
            y-coll)))


(defn rotate
  [coll
   distance]
  (let [total (count coll)
        idx (range total)
        zmap (zipmap idx
                     coll)]
    (reduce
      (fn [acc coll]
        (assoc
          acc
          (get coll 0)
          (get coll 1)))
      []
      (sort (map (fn [a]
                   [(mod (+ distance
                            (get a 0))
                         total)
                    (get a 1)])
                 zmap)))))

(defn
  apply-instruction
  [board
   instruction]
  (condp = (:instruction-type instruction)
    :rect (reduce (fn [board [x y]]
                    (assoc-in
                      board
                      [x y]
                      1))
                  board
                  (gen-board-positions (:width instruction)
                                       (:height instruction)))
    :rotate-column (let [col-idx (:column instruction)
                         distance (:distance instruction)]
                     (assoc
                       board
                       col-idx
                       (rotate (get board col-idx)
                               distance)))
    :rotate-row (board-transpose (let [row-idx (:row instruction)
                                       distance (:distance instruction)]
                                   (assoc
                                     (board-transpose board)
                                     row-idx
                                     (rotate (get (board-transpose board) row-idx)
                                             distance))))))

(defn line->map
  [line]
  (let [[instruction-type
         remaining-instructions] (case (first line)
                                   "rect" [:rect
                                           (rest line)]
                                   "rotate" (case (second line)
                                              "row" [:rotate-row
                                                     (drop 2 line)]
                                              "column" [:rotate-column
                                                        (drop 2 line)]))
        [k1 k2 [v1 v2]] (case instruction-type
                          :rect [:width
                                 :height
                                 (map #(Integer/parseInt %)
                                      (str/split (first remaining-instructions)
                                                 #"x"))]
                          :rotate-row [:row
                                       :distance
                                       (map #(Integer/parseInt %)
                                            [(re-find #"\d+" (first remaining-instructions))
                                             (last remaining-instructions)])]
                          :rotate-column [:column
                                          :distance
                                          (map #(Integer/parseInt %)
                                               [(re-find #"\d+" (first remaining-instructions))
                                                (last remaining-instructions)])])]
    {:instruction-type instruction-type
     k1 v1
     k2 v2}))

(defn parser
  [line]
  (line->map (str/split line #" ")))

(defn case-1
  [input-path]
  (count (remove zero?
                 (mapcat concat
                         (board-transpose
                           (reduce (fn [acc i]
                                     (apply-instruction acc i))
                                   (gen-board 50 6)
                                   (into []
                                         (map parser
                                              (str/split-lines (slurp input-path))))))))))

(defn case-2
  [input-path]
  (board-transpose
    (reduce (fn [acc i]
              (apply-instruction acc i))
            (gen-board 50 6)
            (map parser
                 (str/split-lines (slurp input-path))))))

(into []
      (map (fn [a]
             (into []
                   (map (fn [b]
                          (case b
                            0 "_"
                            1 "#"))
                        a)))
           (board-transpose
             (reduce (fn [acc i]
                       (apply-instruction acc i))
                     (gen-board 50 6)
                     (map parser
                          (str/split-lines (slurp "inputs/day8.txt")))))))

[["_" "#" "#" "_" "_" "#" "#" "#" "#" "_" "#" "#" "#" "_" "_" "#" "_" "_" "#" "_" "#" "#" "#" "_" "_" "#" "#" "#" "#" "_" "#" "#" "#" "_" "_" "_" "_" "#" "#" "_" "#" "#" "#" "_" "_" "_" "#" "#" "#" "_"]
 ["#" "_" "_" "#" "_" "#" "_" "_" "_" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "_" "_" "_" "#" "_" "#" "_" "_" "#" "_" "_" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "_" "_" "_" "_"]
 ["#" "_" "_" "#" "_" "#" "#" "#" "_" "_" "#" "#" "#" "_" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "_" "_" "#" "_" "_" "#" "#" "#" "_" "_" "_" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "_" "_" "_" "_"]
 ["#" "#" "#" "#" "_" "#" "_" "_" "_" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "#" "#" "_" "_" "_" "#" "_" "_" "_" "#" "_" "_" "#" "_" "_" "_" "_" "#" "_" "#" "#" "#" "_" "_" "_" "#" "#" "_" "_"]
 ["#" "_" "_" "#" "_" "#" "_" "_" "_" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "_" "_" "_" "_" "#" "_" "_" "_" "_" "#" "_" "_" "#" "_" "#" "_" "_" "#" "_" "#" "_" "_" "_" "_" "_" "_" "_" "#" "_"]
 ["#" "_" "_" "#" "_" "#" "_" "_" "_" "_" "#" "#" "#" "_" "_" "_" "#" "#" "_" "_" "#" "_" "_" "_" "_" "#" "#" "#" "#" "_" "#" "#" "#" "_" "_" "_" "#" "#" "_" "_" "#" "_" "_" "_" "_" "#" "#" "#" "_" "_"]]
;AFBUPZBJPS


