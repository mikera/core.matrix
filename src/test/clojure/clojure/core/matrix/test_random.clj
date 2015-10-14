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

(deftest test-sample-rand-int
  (testing "Integer results"
    (is (every? integer? (eseq (sample-rand-int [5 5] 10)))))
  (testing "Edge cases"
    (is (zero-matrix? (sample-rand-int [5 5] 0)))
    (is (zero-matrix? (sample-rand-int [5 5] 0.5)))
    (is (zero-matrix? (sample-rand-int [5 5] 1)))
    (is (not (zero-matrix? (sample-rand-int [5 5] 1.5 567))))))

(deftest test-random-sequence-seeds
  (is (= (take 1000 (randoms 1337)) (take 1000 (randoms 1337))))) 

(deftest test-sample-seed
  (testing "Same seed should produce same result"
    (is (equals (sample-uniform 10 890) (sample-uniform 10 890)))
    (is (equals (sample-rand-int 10 100 890) (sample-rand-int 10 100 890)))
    (is (equals (sample-normal 10 123) (sample-normal 10 123)))
    (is (equals (sample-binomial 10 0.5 10 123) (sample-binomial 10 0.5 10 123))))
  (testing "Different seed should produce different result"
    (is (not (equals (sample-uniform 10 890) (sample-uniform 10 4353))))
    (is (not (equals (sample-rand-int 10 100 890) (sample-rand-int 10 100 3453))))
    (is (not (equals (sample-normal 10 123) (sample-normal 10 12))))
    (is (not (equals (sample-binomial 10 0.5 10 123) (sample-binomial 10 0.5 10 1234)))))
  (testing "No seed should produce different result"
    (is (not (equals (sample-uniform 10) (sample-uniform 10))))
    (is (not (equals (sample-rand-int 10 100) (sample-rand-int 10 100))))
    (is (not (equals (sample-normal 10) (sample-normal 10))))
    (is (not (equals (sample-binomial 10 0.5 10) (sample-binomial 10 0.5 10))))
    (is (not (equals (sample-binomial 100 0.5) (sample-binomial 100 0.5))))))
