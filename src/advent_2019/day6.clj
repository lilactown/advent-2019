(ns advent-2019.day6)
(require '[clojure.string :as string])

(defn orbits [input]
  (loop [lines (string/split input #"\n")
         orbs {}]
    (if lines
      (let [[lhs rhs] (string/split (first lines) #"\)")]
        (recur (next lines)
               (update orbs lhs (fnil conj #{}) rhs)))
      orbs)))

(defn count-all-edges [edges root depth]
  (+ depth (reduce (fn [c node]
                     (+ c (count-all-edges edges node (inc depth))))
                   0
                   (edges root))))

(require '[clojure.java.io :as io])

(def input (-> (io/resource "day6.input")
               (slurp)))

(prn ::part1 (-> input
                 (orbits)
                 (count-all-edges "COM" 0)))

(defn invert-orbits [input]
  (loop [lines (string/split input #"\n")
         orbs {}]
    (if lines
      (let [[lhs rhs] (string/split (first lines) #"\)")]
        (recur (next lines)
               (update orbs rhs (fnil conj #{}) lhs)))
      orbs)))

(defn path [edges root]
  ;; by inverting the orbits, we guarantee that edges will only ever have one leaf
  (let [node (first (edges root))]
    (cons root
          (if (nil? node)
            nil
            (path edges node)))))

(defn first-common-node [p1 p2]
  (loop [p1 p1
         d1 -1] ;; don't count first node
    (when p1
      (if-let [found (some #{(first p1)} p2)]
        `(~found ~d1)
        (recur (next p1)
               (inc d1))))))

(def you (-> (io/resource "day6.input")
             (slurp)
             (invert-orbits)
             (path "YOU")))

(def santa (-> (io/resource "day6.input")
               (slurp)
               (invert-orbits)
               (path "SAN")))



(first-common-node you santa)
;; => ("59F" 288)

(first-common-node santa you)
;; => ("59F" 259)

(prn ::part2 (+ 288 259))
;; => 547
