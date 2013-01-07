(ns core.matrix.test-operators
  (:use clojure.test)
  (:use core.matrix)
  (:use core.matrix.operators)
  (:require core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector? * - + /]))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (* 3 2))))
  (testing "matrix scaling"
    (is (= [6.0] (* [3] 2)))
    (is (= [6.0] (* 2 [3])))
    (is (= [[6.0]] (* 2 [[3]])))
    (is (= [[6.0]] (* [[2]] 3)))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (* 3 2))))
  (testing "matrix scaling"
    (is (= [6.0] (* [3] 2)))
    (is (= [6.0] (* 2 [3])))
    (is (= [[6.0]] (* 2 [[3]])))
    (is (= [[6.0]] (* [[2]] 3)))))

(deftest test-maths-ops
  (testing "scalars"
    (is (== 1 (signum 2.5)))
    (is (== 1.0 (round 0.8))))
  (testing "matrices"
    (is (= [1.0] (signum [1.3])))
    (is (= [-1.0] (signum [-100])))))

(deftest test-subtraction
  (testing "scalars"
    (is (== 5 (- 7 2)))
    (is (== 6 (- 10 2 2))))
  (testing "matrix subtraction"
    (is (= [1.0] (- [3.0] [2.0])))
    (is (= [[8.0]] (- [[12.0]] [[4.0]])))
    (is (= [[[8.0]]] (- [[[12.0]]] [[[4.0]]])))))
