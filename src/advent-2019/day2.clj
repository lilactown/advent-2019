(ns advent-2019.day2)
(def ops {99 :halt
          1  :add
          2  :multiply})

(defn parameters [program address]
  (let [arg1-address (get program (+ 1 address))
        arg2-address (get program (+ 2 address))
        result-address (get program (+ 3 address))]
    [(get program arg1-address)
     (get program arg2-address)
     result-address]))

(defmulti -instruction (fn [op-name _ _ _ _]
               op-name))

(defn do-instruction [program address]
  (let [opcode (get program address)
        op-name (ops opcode)]
    (let [[arg1 arg2 result-address] (parameters program address)]
      (-instruction op-name arg1 arg2 result-address program))))

(defmethod -instruction :halt [_ _ _ _ _]
  nil)

(defmethod -instruction :add [_ arg1 arg2 result-address program]
  (assoc program result-address (+ arg1 arg2)))

(defmethod -instruction :multiply [_ arg1 arg2 result-address program]
  (assoc program result-address (* arg1 arg2)))

(assert (= (do-instruction [1,9,10,3,2,3,11,0,99,30,40,50] 0)
           [1,9,10,70,
            2,3,11,0,
            99,
            30,40,50]))

(defn run-seq
  ([program] (cons program (run-seq program 0)))
  ([program address]
   (lazy-seq
    (let [program' (do-instruction program address)]
      (cons program'
            (run-seq program' (+ address 4)))))))

(def test-seq (run-seq [1,9,10,3,2,3,11,0,99,30,40,50]))
;; explodes 😬
(try (take 10 test-seq)
     (catch Exception e
       (println e)))

;; let's debug

(defmacro trivially-catch [& body]
  `(try ~@body
        (catch Exception e#
          (prn (ex-message e#)))))

(trivially-catch
 (prn (first test-seq))) ;; 👍

(trivially-catch
 (prn (first (rest test-seq)))) ;; 👍

(trivially-catch
 (prn (first (rest (rest test-seq))))) ;; 👍

(trivially-catch
 (prn (first (rest (rest (rest test-seq)))))) ;; 💥

;; fix when opcode is `nil`
(defn do-instruction [program address]
  (when-let [opcode (get program address)]
    (let [op-name (ops opcode)
          [arg1 arg2 result-address] (parameters program address)]
     (-instruction op-name arg1 arg2 result-address program))))

(defn run-until-halt [program]
  (last (take-while (comp not nil?) (run-seq program))))

(assert (-> (run-until-halt [1,9,10,3,2,3,11,0,99,30,40,50])
            (= [3500,9,10,70,
                2,3,11,0,
                99,
                30,40,50])))

(assert (-> (run-until-halt [1,0,0,0,99])
            (= [2,0,0,0,99])))

(assert (-> (run-until-halt [2,3,0,3,99])
            (= [2,3,0,6,99])))

(assert (-> (run-until-halt [2,4,4,5,99,0])
            (= [2,4,4,5,99,9801])))

(assert (-> (run-until-halt [1,1,1,4,99,5,6,0,99])
            (= [30,1,1,4,2,5,6,0,99])))

(require '[clojure.java.io :as io])

(require '[clojure.string :as string])

(def input (-> (io/resource "day2.input")
               (slurp)
               (string/split #",")
               (->> (mapv #(Integer/parseInt %)))))

(println ::part1 (time (-> input
                           (assoc 1 12
                                  2 2)
                           (run-until-halt)
                           (get 0))))

(defn result-with-inputs [program noun verb]
  (-> program
      (assoc 1 noun
             2 verb)
      (run-until-halt)
      (get 0)))

(defn results [program]
  (for [noun (range 0 168)
        verb (range 0 168)]
    ;; return the memory, and the current noun and verb in a vector
    [(result-with-inputs program noun verb) noun verb]))

;; set the search result 1 above 19690720 so that it will output the result
(let [[_ noun verb] (time
                     (last
                      (take-while
                       (comp #(not= 19690721 %) first)
                       (results input))))]
  (println ::part2 (+ (* 100 noun) verb)))
