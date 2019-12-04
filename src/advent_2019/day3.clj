(ns advent-2019.day3)
(defn segment [origin desc]
  (let [direction (first desc)
        amount (Integer/parseInt (subs desc 1))
        [x y] origin]
    (case direction
      \R (for [x' (range (inc x) (+ x amount 1))]
               [x' y])
      ;; reverse direction of range so that it will generate a seq from
      ;; origin to -amount
      \L (for [x' (reverse (range (- x amount) x))]
              [x' y])
      \U (for [y' (range (inc y) (+ y amount 1))]
            [x y'])
      \D (for [y' (reverse (range (- y amount) y))]
              [x y']))))

(segment [0 0] "R10")
;; => ([1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0] [9 0] [10 0])
(segment [0 0] "L10")
;; => ([-1 0] [-2 0] [-3 0] [-4 0] [-5 0] [-6 0] [-7 0] [-8 0] [-9 0] [-10 0])
(segment [0 0] "U10")
;; => ([0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8] [0 9] [0 10])
(segment [0 0] "D10")
;; => ([0 -1] [0 -2] [0 -3] [0 -4] [0 -5] [0 -6] [0 -7] [0 -8] [0 -9] [0 -10])

(defn segments
  "Creates a transducer of each segment in a line"
  ([] (segments [0 0]))
  ([origin]
   (fn [rf]
     (let [origin (volatile! origin)]
       (fn
         ([] (rf))
         ([result]
          (rf result))
         ([result input]
          (let [s (segment @origin input)]
            (vreset! origin (last s))
            (rf result s))))))))

(require '[clojure.string :as string])

(defn line [text]
  (->> (string/split text #",")
       (sequence (comp (segments) cat))))

(line "U2,R3")
;; => ([0 1] [0 2] [1 2] [2 2] [3 2])

(defn indistinct
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (if (contains? seen f)
                        (cons f (step (rest s) seen))
                        (recur (rest s) (conj seen f)))))
                  xs seen)))]
     (step coll #{}))))

(defn manhatten
  ([[x y]]
   (+ (Math/abs x)
      (Math/abs y)))
  ([[x1 y1] [x2 y2]]
   (+ (Math/abs (- x1 x2))
      (Math/abs (- y1 y2)))))

(defn part1 [[wire1 wire2]]
  (let [line1 (line wire1)
        line2 (line wire2)]
    ;; `distinct` ensures we don't count wires crossing themselves
    (->> (concat (distinct line1) (distinct line2))
         (indistinct)
         (map manhatten)
         (apply min))))

(part1 ["R8,U5,L5,D3" "U7,R6,D4,L4"])
;; => 6

(part1 ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83"])
;; => 157

(part1 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
;; => 135


(require '[clojure.java.io :as io])

(def input (-> (io/resource "day3.input")
               (slurp)
               (string/split #"\n")))

(prn (time (part1 input)))

;; https://stackoverflow.com/a/4831170/4379329
(defn find-thing [needle haystack]
  (first (keep-indexed #(when (= %2 needle) %1) haystack)))

(defn part2 [[wire1 wire2]]
  (let [line1 (line wire1)
        line2 (line wire2)
        crosses (->> (concat (distinct line1) (distinct line2))
                     (indistinct))]
    (+ 2 ;; add two because the sequence are zero-indexed
       (apply min (for [point crosses]
                    (+ (find-thing point line1)
                       (find-thing point line2)))))))

(part2 ["R8,U5,L5,D3" "U7,R6,D4,L4"])
;; => 30

(part2 ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83"])
;; => 610


(part2 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])
;; => 410

(prn (time (part2 input)))
