(ns clojure.core.matrix.test-vectorz
  (:require [clojure.core.matrix.impl.pprint :as pprint]
            [mikera.vectorz.matrix-api]
            [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix.stats :as stats]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all])
  (:import (mikera.arrayz INDArray)) )

;; Tests for the Vectorz implementation, an important high speed JVM array library
;;
;; Not intended to be exhasustive (since vectorz-clj has its own tests) however
;; this is useful to detect breaking changes early.

(defmacro vectorz?
  [x]
  `(instance? INDArray ~x))

(deftest regression-201
  (let [m (matrix :vectorz [[2 2][2 2]])
        mm (mutable m)]
    (is (vectorz? mm))
    (is (equals [[4 4] [4 4]] (pow! mm 2)))))

(deftest test-sparse
  (is (vectorz? (sparse (matrix :vectorz [[1 2] [3 4]]))))
  (is (vectorz? (sparse :vectorz [[1 2] [3 4]])))
  (is (vectorz? (sparse-array :vectorz [[[1 2] [3 4]]]))))

(deftest test-pm
  (is (string? (pprint/pm (array :vectorz [1 2])))))

(deftest compliance-tests
    ;; TODO: fix issue with validate-shape
    ;;(compliance/instance-test (array :vectorz 1))
    ;;(compliance/instance-test (array :vectorz [1 2 3]))
    ;;(compliance/instance-test (array :vectorz [[1 2] [3 4]]))
    ;;(compliance/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]]))  

    )

(deftest test-stats
  (is (vectorz? (stats/mean [(array :vectorz [1 2])
                             (array :vectorz [5 4])]))))

(deftest test-filter-slices
  (is (equals [[1 2] [3 4]] (filter-slices #(< (esum %) 20)
                                           (array :vectorz [[1 2] [10 20] [3 4]])))))

(deftest test-inner-product
  (let [v (array :vectorz [1 2])]
    (add-inner-product! v [[2 0] [0 3]] [10 20])
    (is (equals [21 62] v))
    (add-inner-product! v [[2 0] [0 3]] [10 20] -1)
    (is (equals [1 2] v)))
  (let [v (array :vectorz [1 2])]
    (set-inner-product! v [[2 0] [0 3]] [10 20])
    (is (equals [20 60] v))
    (set-inner-product! v [[2 0] [0 3]] [10 20] 10.0)
    (is (equals [200 600] v))))

(deftest test-mutable
  (is (vectorz? (mutable :vectorz [1 2 3])))
  (is (not (vectorz? (mutable [1 2 3]))))
  (is (vectorz? (with-implementation :vectorz (mutable [1 2 3]))))
  (is (not (vectorz? (with-implementation :vectorz (mutable [1 2 :d]))))))

(deftest test-emap-indexed
  (is (equals [[1 12] 
               [103 114]] 
              (emap-indexed 
                (fn [[i j] x] (+ x (* 100 i) (* 10 j)))
                (array :vectorz [[1 2] [3 4]])))))
