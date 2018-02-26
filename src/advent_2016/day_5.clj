(ns advent-2016.day-5
  (:import (java.security MessageDigest)
           (java.math BigInteger)))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (javax.xml.bind.DatatypeConverter/printHexBinary raw)))

(defn distinct-by
  [fun]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (println [(fun input)
                   @seen])
         (if (contains? @seen (fun input))
           result
           (do (vswap! seen conj (fun input))
               (rf result input))))))))

(defn case-1-try-2
  [s]
  (apply str
         (sequence (comp (map #(md5 (str s %1)))
                         (filter #(re-matches #"^00000.*$" %1))
                         (map #(nth %1 5))
                         (take 8))
                   (range))))

(defn case-2
  [s]
  (apply str
         (map :char
              (sort-by :index
                       (transduce (comp (map (comp md5
                                                   (partial str s)))
                                        ;(map #(md5 (str s %)))
                                        (filter (partial re-find #"^0{5}[0-7].*$"))
                                        (map (fn [v]
                                               {:index (nth v 5)
                                                :char (nth v 6)}))
                                        (distinct-by :index)
                                        (take 8))
                                  conj
                                  (range))))))
