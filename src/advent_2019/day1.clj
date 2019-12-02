(ns advent-2019.day1)
(require '[clojure.java.io :as io])

(require '[clojure.string :as string])

(def input (-> (io/resource "day1.input")
               (slurp)
               (string/split #"\n")
               (->> (map #(Integer/parseInt %)))))

(defn fuel [mass]
  (-> mass (/ 3) (Math/floor) (- 2) (int)))

(def part1 (transduce (map fuel) + input))

(println ::part1 part1)

(defn fuel2
  ([mass]
   ;; we don't add the initial mass. we just want the fuel's mass
   ;; so we initialize the loop with the mass of the fuel
   (fuel2 (fuel mass) 0))
  ([mass sum]
   (if (<= mass 0)
     sum
     (recur (fuel mass)
            (+ sum mass)))))

(assert (= (fuel2 1969) 966))

(assert (= (fuel2 100756) 50346))

(def part2 (transduce (map fuel2) + input))

(println ::part2 part2)
