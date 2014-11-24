(ns clojure.core.matrix.test-vectorz
  (:require [clojure.core.matrix.impl.pprint :as pprint]
            [mikera.vectorz.matrix-api]
            [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all])
  (:import (mikera.arrayz INDArray)) )

;; Tests for the Vectorz implementation, an important high speed JVM array library
;;
;; Not intended to be exhasustive (since vectorz-clj has its own tests) however
;; this is useful to detect breaking changes early.

(deftest regression-201
  (let [m (matrix :vectorz [[2 2][2 2]])
        mm (mutable m)]
    (is (instance? INDArray mm))
    (is (equals [[4 4] [4 4]] (pow! mm 2)))))

(deftest test-sparse
  (is (instance? INDArray (sparse (matrix :vectorz [[1 2] [3 4]]))))
  (is (instance? INDArray (sparse :vectorz [[1 2] [3 4]]))))

(deftest test-pm
  (is (string? (pprint/pm (array :vectorz [1 2])))))

(deftest compliance-tests
  (compliance/instance-test (array :vectorz [1 2 3]))
  (compliance/instance-test (array :vectorz [[1 2] [3 4]]))
  (compliance/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]])))
