(ns clojure.core.matrix.test-random
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.random)
  (:use clojure.test))

(deftest test-sample-uniform
  (is (= [10] (shape (sample-uniform 10))))
  (is (= [10] (shape (sample-uniform [10]))))
  (is (number? (scalar (sample-uniform [])))))

(deftest test-random-sequence
  (let [rs (randoms)]
    (is (= (take 1000 rs) (take 1000 rs)))
    (is (<= 0 (reduce + (take 1000 rs)) 1000)))
  (let [rs (nnext (randoms))]
    (is (= (take 1000 rs) (take 1000 rs)))
    (is (<= 0 (reduce + (take 1000 rs)) 1000)))) 

(deftest test-random-sequence-seeds
  (is (= (take 1000 (randoms 1337)) (take 1000 (randoms 1337))))) 
