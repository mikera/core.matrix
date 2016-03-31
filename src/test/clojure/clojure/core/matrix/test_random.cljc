(ns clojure.core.matrix.test-random
  (:require [clojure.core.matrix :as mat]
            [clojure.core.matrix.random :as r]
    #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])))

(deftest test-sample-uniform
  (is (= [10] (mat/shape (r/sample-uniform 10))))
  (is (= [10] (mat/shape (r/sample-uniform [10]))))
  (is (number? (mat/scalar (r/sample-uniform []))))
  (is (< 0 (mat/emin (r/sample-uniform [10]))))
  (is (> 1 (mat/emax (r/sample-uniform [10])))))

(deftest test-sample-normal
  (is (= [10] (mat/shape (r/sample-normal 10))))
  (is (= [10] (mat/shape (r/sample-normal [10]))))
  (is (number? (mat/scalar (r/sample-normal []))))
  (is (> 0 (mat/emin (r/sample-normal [100]))))
  (is (< 0 (mat/emax (r/sample-normal [100])))))

(deftest test-sample-uniform
  (is (= [10] (mat/shape (r/sample-uniform 10))))
  (is (= [10] (mat/shape (r/sample-uniform [10]))))
  (is (number? (mat/scalar (r/sample-uniform [])))))

(deftest test-random-sequence
  (let [rs (r/randoms)]
    (is (= (take 1000 rs) (take 1000 rs)))
    (is (<= 0 (reduce + (take 1000 rs)) 1000)))
  (let [rs (nnext (r/randoms))]
    (is (= (take 1000 rs) (take 1000 rs)))
    (is (<= 0 (reduce + (take 1000 rs)) 1000))))

(deftest test-sample-rand-int
  (testing "Integer results"
    (is (every? integer? (mat/eseq (r/sample-rand-int [5 5] 10)))))
  (testing "Edge cases"
    (is (mat/zero-matrix? (r/sample-rand-int [5 5] 0)))
    (is (mat/zero-matrix? (r/sample-rand-int [5 5] 0.5)))
    (is (mat/zero-matrix? (r/sample-rand-int [5 5] 1)))
    (is (not (mat/zero-matrix? (r/sample-rand-int [5 5] 1.5 567))))))

#?(:clj (do

(deftest test-random-sequence-seeds
  (is (= (take 1000 (r/randoms 1337)) (take 1000 (r/randoms 1337)))))

(deftest test-sample-seed
  (testing "Same seed should produce same result"
    (is (mat/equals (r/sample-uniform 10 890) (r/sample-uniform 10 890)))
    (is (mat/equals (r/sample-rand-int 10 100 890) (r/sample-rand-int 10 100 890)))
    (is (mat/equals (r/sample-normal 10 123) (r/sample-normal 10 123)))
    (is (mat/equals (r/sample-binomial 10 0.5 10 123) (r/sample-binomial 10 0.5 10 123))))
  (testing "Different seed should produce different result"
    (is (not (mat/equals (r/sample-uniform 10 890) (r/sample-uniform 10 4353))))
    (is (not (mat/equals (r/sample-rand-int 10 100 890) (r/sample-rand-int 10 100 3453))))
    (is (not (mat/equals (r/sample-normal 10 123) (r/sample-normal 10 12))))
    (is (not (mat/equals (r/sample-binomial 10 0.5 10 123) (r/sample-binomial 10 0.5 10 1234)))))
  (testing "No seed should produce different result"
    (is (not (mat/equals (r/sample-uniform 10) (r/sample-uniform 10))))
    (is (not (mat/equals (r/sample-rand-int 10 100) (r/sample-rand-int 10 100))))
    (is (not (mat/equals (r/sample-normal 10) (r/sample-normal 10))))
    (is (not (mat/equals (r/sample-binomial 10 0.5 10) (r/sample-binomial 10 0.5 10))))
    (is (not (mat/equals (r/sample-binomial 100 0.5) (r/sample-binomial 100 0.5))))))

))

