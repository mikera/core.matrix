(ns core.matrix.test-api
  (:use clojure.test)
  (:use core.matrix)
  (:require core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (mget [1 2 3] 0)))
    (is (== 1 (mget [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (mget [[[1 2] [3 4]] [[5 6] [7 8]]] 1 1 1)))))

(deftest test-coerce
  (testing "clojure vector"
    (is (= [1 2 3] (coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (coerce [1] [[1 2] [3 4]])))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (mul 3 2))))
  (testing "matrix scaling"
    (is (= [6.0] (mul [3] 2)))
    (is (= [6.0] (mul 2 [3])))
    (is (= [[6.0]] (mul 2 [[3]])))
    (is (= [[6.0]] (mul [[2]] 3)))))

(deftest test-dimensions
  (testing "vector dimensions"
    (is (= 3 (row-count [1 2 3])))
    (is (= 3 (row-count [[1 2] [2 3] [3 4]])))
    (is (= 2 (column-count [[1 2] [2 3] [3 4]])))
    (is (= [3 2] (all-dimensions [[1 2] [2 3] [3 4]])))
    (is (= [2 2 2] (all-dimensions [[[1 2] [2 3]] [[3 4] [5 6]]])))))

(deftest test-predicates
  (testing "clojure vector predicates"
    (is (matrix? [1 2]))
    (is (vector? [1 2]))
    (is (matrix? [[1 2] [3 4]]))
    (is (matrix-2d? [[1 2] [3 4]]))
    (is (not (vector? [[1 2] [3 4]])))
    (is (not (matrix-2d? [[[1 2] [2 3]] [[3 4] [5 6]]]))))
  (testing "row and column predicates"
    (is (column-matrix? [1]))
    (is (column-matrix? [[1]]))
    (is (row-matrix? [1]))
    (is (row-matrix? [[1]]))
    (is (column-matrix? [1 2]))
    (is (column-matrix? [[1] [2]]))
    (is (row-matrix? [[1 2 3]]))
    (is (not (row-matrix? [1 2])))))