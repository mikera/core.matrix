(ns clojure.core.matrix.test-vectorz
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [mikera.vectorz.matrix-api])
  (:use clojure.test))

;; Tests for the Vectorz implementation, an important high speed JVM array library
;;
;; Not intended to be exhasustive (since vectorz-clj has its own tests) however
;; this is useful to detect breaking changes early.

(deftest regression-201
  (let [m (matrix :vectorz [[2 2][2 2]])
        mm (mutable m)]
    (is (instance? mikera.arrayz.INDArray mm))
    (is (equals [[4 4] [4 4]] (pow! mm 2)))))

(deftest test-sparse
  (is (instance? mikera.arrayz.INDArray (sparse (matrix :vectorz [[1 2] [3 4]])))))

(deftest test-pm
  (is (string? (clojure.core.matrix.impl.pprint/pm (array :vectorz [1 2]))))) 

(deftest compliance-tests
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[1 2] [3 4]]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]])))