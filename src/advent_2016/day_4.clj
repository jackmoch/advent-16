(ns advent-2016.day-4
  (:require [clojure.string :as str]))

(defn highest-letter [m]
  (reduce (fn [x y]
            (let [xf (val x)
                  yf (val y)]
              (cond (> xf yf) x
                    (< xf yf) y
                    (= xf yf) (let [c (compare (key x) (key y))]
                                (if (< c 0)
                                  x
                                  y)))))
          m))

(defn f-map->checksum [f-map]
  (:acc
    (reduce (fn [{:keys [acc f-map] :as e} _]
              (let [current-letter (first (highest-letter f-map))]
                (assoc
                  e
                  :acc (str acc current-letter)
                  :f-map (dissoc f-map current-letter))))
            {:acc ""
             :f-map f-map}
            (range 5))))

(defn parse-room [room]
  (let [chars (apply str
                     (str/split (re-find #"[a-z\-]*" room)
                                #"-"))
        checksum (get (re-find #"\[([a-z]*)\]" room) 1)
        room-num (re-find #"\d+" room)]
    {:string (get (re-find #"([a-z\-]*)\-\d+" room) 1)
     :chars chars
     :checksum checksum
     :my-checksum (f-map->checksum (frequencies chars))
     :f-map (frequencies chars)
     :room-num (Integer/parseInt room-num)}))

(defn decrypt
  [{:keys [string room-num]}]
  (apply str (map (fn [l]
                    (if (= \- l)
                      l
                      (let [char->num (zipmap (map char (range (int \a) (inc (int \z))))
                                              (range 0 26))
                            num->char (zipmap (range 0 26)
                                              (map char (range (int \a) (inc (int \z)))))
                            new-letter (num->char (mod (+ (char->num l)
                                                          room-num)
                                                       26))]
                        new-letter)))
                  string)))

(defn case-2-pass-2
  [input-path]
  (let [parsed-rooms (map parse-room
                          (str/split-lines (slurp input-path)))]
    (filter (fn [{:keys [room-num
                         decrypted]}]
              (re-find #"(north).+" decrypted))
            (map (fn [rm]
                   (assoc
                     rm
                     :decrypted
                     (decrypt rm)))
                 parsed-rooms))))


(defn case-1-pass-2
  [input-path]
  (apply +
         (map :room-num
              (filter
                (fn [{:keys [checksum
                             my-checksum]}]
                  (= checksum
                     my-checksum))
                (map parse-room
                     (str/split (slurp input-path)
                                #"\n"))))))
