(ns advent-2019.intcode-test)
(require '[clojure.test :as t])

(require '[advent-2019.intcode :as i])

(t/deftest operation
  (t/is (i/operation ["3"] 0) '[() 3])
  (t/is (i/operation ["1101"] 0) '[(1 1) 1])
  (t/is (i/operation ["11101"] 0) '[(1 1 1) 1])
  (t/is (i/operation ["99"] 0) '[() 99]))
(t/deftest pad
  (t/is (i/pad [1 1] 3) '(0 1 1))
  (t/is (i/pad '(1) 3) '(0 0 1))
  (t/is (i/pad '(1 0 1 1) 3) '(1 0 1 1)))

(t/deftest parameters
  (t/is (i/parameters ["1002" "4" "3" "4" "33"] 0 '(0 1 1))
        '(33 3 4)))

(t/deftest compute
  (t/is
   (-> "1002,4,3,4,33"
       (i/create-program "1")
       (i/compute)
       (= {:memory ["1002" "4" "3" "4" "99"],
           :pointer 4,
           :input "1",
           :output [],
           :running? true})))

  (t/is
   (-> "3,0,4,0,99"
       (i/create-program "1")
       (i/compute)
       (i/compute)
       (i/compute)
       (= {:memory ["1" "0" "4" "0" "99"], :pointer 4, :input "1", :output [1], :running? false}))))

(t/deftest run-simple
  (t/is (-> "3,0,4,0,99"
            (i/run "1")
            (= [1]))))

(t/deftest run
  (t/testing "equal to position test"
    (t/is (-> "3,9,8,9,10,9,4,9,99,-1,8"
              (i/run "8")
              (last)
              (= 1)))

    (t/is (-> "3,9,8,9,10,9,4,9,99,-1,8"
              (i/run "9")
              (last)
              (= 0))))

  (t/testing "less than position test"
    (t/is (-> "3,9,7,9,10,9,4,9,99,-1,8"
              (i/run "4")
              (last)
              (= 1)))

    (t/is (-> "3,9,7,9,10,9,4,9,99,-1,8"
              (i/run "9")
              (last)
              (= 0))))

  (t/testing "equal to immediate test"
    (t/is (-> "3,3,1108,-1,8,3,4,3,99"
              (i/run "8")
              (last)
              (= 1)))

    (t/is (-> "3,3,1108,-1,8,3,4,3,99"
              (i/run "9")
              (last)
              (= 0))))

  (t/testing "less than immediate test"
    (t/is (-> "3,3,1107,-1,8,3,4,3,99"
              (i/run "4")
              (last)
              (= 1)))

    (t/is (-> "3,3,1107,-1,8,3,4,3,99"
              (i/run "9")
              (last)
              (= 0))))

  (t/testing "jump test"
    (t/is (-> "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
              (i/run "0")
              (last)
              (= 0)))

    (t/is (-> "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
              (i/run "1")
              (last)
              (= 1))))

  (t/testing "jump immediate"
    (-> "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        (i/run "1")
        (last)
        (= 1))

    (-> "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        (i/run "0")
        (last)
        (= 0)))

  (t/testing "large example"
    (let [large-example "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
      (t/is (-> large-example
                (i/run "4")
                (last)
                (= 999)))

      (t/is (-> large-example
                (i/run "8")
                (last)
                (= 1000)))

      (t/is (-> large-example
                (i/run "9")
                (last)
                (= 1001))))))
