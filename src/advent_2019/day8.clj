(ns advent-2019.day8)
(defn char->int [c]
  (Integer/parseInt (String/valueOf c)))

(defn layers [height width pixels]
  (->> pixels
       (map char->int)
       (partition (* height width))))

(defn zeros [layer]
  (count (filter zero? layer)))

(require '[clojure.java.io :as io])

(prn ::part1
     (let [layer (->> (io/resource "day8.input")
                      (slurp)
                      (layers 6 25)
                      (apply min-key zeros))
           ones (->> layer
                     (filter #(= 1 %))
                     (count))
           twos (->> layer
                     (filter #(= 2 %))
                     (count))]
       (* ones twos)))

(defn choose-px [pixels]
  (if (= 1 (reduce (fn [cur px]
                     (if (#{0 1} cur)
                       (reduced cur)
                       px))
                   pixels))
    \█
    \space))

(defn transpose [layers]
  (apply map list layers))

(require '[clojure.string :as string])

(defn image [height width pixels]
  (->> pixels
       (layers height width)
       (transpose)
       (map choose-px)
       (partition width)
       (map #(string/join %))))

(require '[clojure.pprint :refer [pprint]])

(prn ::part2)
(pprint (->> (io/resource "day8.input")
             (slurp)
             (image 6 25)))
