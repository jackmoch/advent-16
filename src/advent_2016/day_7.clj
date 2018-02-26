(ns advent-2016.day-7
  (:require [clojure.string :as str]))

(defn
  abba-checker
  [[c1 c2 c3 c4]]
  (and
    (not= c1 c2)
    (= (str c1 c2)
       (str c4 c3))))

(defn aba-checker
  [[c1 c2 c3]]
  (if (and (not= c1 c2)
           (= c1
              c3))
    (str c1 c2 c3)))

(defn aba->bab
  [[c1 c2 _]]
  (str c2 c1 c2))

(defn supernet-abba?
  [{:keys [supernet]}]
  (some true?
        (map (fn [a]
               (some true?
                     (map abba-checker
                          a)))
             supernet)))

(defn hypernet-abba?
  [{:keys [hypernet]}]
  (some true?
        (map (fn [a]
               (some true?
                     (map abba-checker
                          a)))
             hypernet)))

(defn abba? [nets])

(defn parser-1
  [s]
  (let [nhn (re-seq #"^[a-z]+(?=\[)|(?<=\])[a-z]+(?=\[)|(?<=\])[a-z]+$" s)
        hn (re-seq #"(?<=\[)[a-z]+(?=\])" s)]
    {:hypernet (map (partial partition 4 1)
                    hn)
     :supernet (map (partial partition 4 1)
                    nhn)}))

(defn parser-2
  [s]
  (let [nhn (re-seq #"^[a-z]+(?=\[)|(?<=\])[a-z]+(?=\[)|(?<=\])[a-z]+$" s)
        hn (re-seq #"(?<=\[)[a-z]+(?=\])" s)]
    {:hypernet (mapcat (partial partition 3 1)
                       hn)
     :supernet (mapcat (partial partition 3 1)
                       nhn)}))

(defn case-2
  [input-path]
  (count (remove empty? (keep
                          (fn [ip]
                            (let [parsed-ip (parser-2 ip)
                                  hypernet (:hypernet parsed-ip)
                                  supernet (:supernet parsed-ip)
                                  aba (remove nil? (map aba-checker
                                                        supernet))
                                  bab (remove nil? (map aba-checker
                                                        hypernet))]
                              (when (seq (filter #(get (set (map aba->bab
                                                                 aba))
                                                       %)
                                                 bab))
                                "YAS")
                              (remove false? (map (partial contains? (set (map aba->bab
                                                                               aba)))
                                                  bab))))
                          (str/split-lines (slurp input-path))))))

(defn case-1
  [input-path]
  (map (fn [a]
         (and (abba? (:hypernet a))
              (complement abba? (:supernet)))))
  (map (comp abba? :hypernet) input)
  (count (filter supernet-abba?
                 (filter (complement hypernet-abba?)
                         (map parser-1
                              (str/split-lines (slurp input-path)))))))
