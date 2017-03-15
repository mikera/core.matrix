(ns clojure.core.matrix.test-numbers
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.macros-clj :refer [error?]]
            [clojure.core.matrix.protocols :as mp]
            [clojure.test :refer :all]))

;; Tests for core.matrix functions on regular scalar numerical values

(deftest test-scalar-properties
  (is (not (array? 1)))
  (is (nil? (shape 1))))

(deftest test-sparse-dense
  (is (== 1 (sparse 1)))
  (is (== 2 (dense 2))))

(deftest test-arithmentic
  (is (== 2 (add 1 1)))
  (is (== 2 (sub 5 3)))
  (is (== 12 (mul 3 4))))

(deftest test-mutable-failure
  (is (error? (add! 1 1)))
  (is (error? (sub! 1 1)))
  (is (error? (mul! 1 1))))

(deftest test-errors
  (is (error? (slices 7)))
  (is (error? (slice-views 7))))

(deftest test-dot
  (is (== 10 (dot 2 5)))
  (is (equals [3 6 9] (dot 3 [1 2 3]))))

(deftest test-rotate
  (testing "Rotate should be identity on scalar values"
    (is (== 3 (rotate 3 0 10)))
    (is (== 3 (rotate 3 10 10)))))

(deftest test-shape
  (is (== 1 (ecount 13)))
  (is (nil? (shape 13))))

(deftest test-magnitude
  (is (== 0 (magnitude 0.0)))
  (is (== 1 (magnitude 1)))
  (is (== 1 (magnitude -1)))
  (is (== 4 (magnitude-squared 2)))
  (is (== 4 (magnitude-squared -2))))

(deftest test-min-max
  (is (== 3 (emin 3)))
  (is (== 2 (emax 2)))
  (is (== 2 (clamp 1 2 5)))
  (is (== 5 (clamp 8 2 5)))
  (is (error? (clamp 3 6 1))))

(deftest test-compute-matrix
  (is (equals 3 (compute-matrix [] (fn [] 3)))))

(deftest test-broadcasting
  (is (equals [2 2 2] (broadcast 2 [3])))
  (is (equals [2] (as-vector 2))))

(deftest test-select
  (testing "mp/select on numbers with a non empty argument should throw an error"
    (is (thrown? RuntimeException
                 (mp/select 12 [[]]))))
  (testing "calling select on numbers with an empty argument should return itself"
    (is (= (select 1) 1))
    (is (= (mp/select 0 []) 0))))

(deftest instance-tests
  (compliance/instance-test 0)
  (compliance/instance-test 1))
