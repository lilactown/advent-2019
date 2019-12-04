(ns advent-2019.day4)
(require '[clojure.spec.alpha :as s])

(quot 923456 100000)

(s/def ::length
  #_#(= 6 (count (str %)))
  #(let [q (quot % 100000)]
     (and (< 0 q)
          (> 10 q))))

(s/valid? ::length 523456)
;; => true

(s/valid? ::length 123)
;; => false

(s/valid? ::length 1234567)
;; => false

;; https://stackoverflow.com/a/29942388/4379329
(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(digits 1234)

(s/def ::adjacent-dupe
  #(let [d (digits %)]
     (not= d (dedupe d))))

(s/valid? ::adjacent-dupe 1212234)
;; => true

(s/valid? ::adjacent-dupe 121234)
;; => false

(s/def ::monotonic
  #(apply <= (digits %)))

(s/valid? ::monotonic 1111)
;; => true

(s/valid? ::monotonic 1211)
;; => false

(s/valid? ::monotonic 1123448)
;; => true

(s/def ::password
  (s/and ::length ::adjacent-dupe ::monotonic))

;; test cases
(s/valid? ::password 111111)
;; => true

(s/valid? ::password 223450)
;; => false

(s/valid? ::password 123789)
;; => false


(print ::part1 (time (count (sequence (filter #(s/valid? ::password %))
                                      (range 168630 718098)))))

(s/def ::adjacent-dupe
  (fn [n]
    (some #(= 2 (second %))
          (frequencies (digits n)))))

(s/valid? ::adjacent-dupe 112233)
;; => true

(s/valid? ::adjacent-dupe 123444)
;; => false

(s/valid? ::adjacent-dupe 111122)
;; => true

(s/def ::password
  (s/and ::length ::adjacent-dupe ::monotonic))

(print ::part2 (time (count (sequence (filter #(s/valid? ::password %))
                                      (range 168630 718098)))))
