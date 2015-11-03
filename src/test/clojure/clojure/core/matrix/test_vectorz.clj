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
  (compliance/instance-test (array :vectorz [1 2 3]))
  (compliance/instance-test (array :vectorz [[1 2] [3 4]]))
  (compliance/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]])))

(deftest test-stats
  (is (vectorz? (stats/mean [(array :vectorz [1 2])
                             (array :vectorz [5 4])]))))

(deftest test-emap-indexed
  (is (equals [[1 12] 
               [103 114]] 
              (emap-indexed 
                (fn [[i j] x] (+ x (* 100 i) (* 10 j)))
                (array :vectorz [[1 2] [3 4]])))))
