(ns clojure.core.matrix.test-utils
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require clojure.core.matrix.examples)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

;; Tests for clojure.core.matrix.utils functions and macros

(deftest test-long-array
  (is (= (type (long-array 0)) (type (long-array nil))))
  (is (= (count (long-array 0)) (count (long-array nil)))))

(deftest test-protocol-extension
  (is (extends-deep? clojure.core.matrix.protocols/PImplementation clojure.lang.PersistentVector))
  (is (extends-deep? clojure.core.matrix.protocols/PImplementation mikera.vectorz.Vector))) 

;; this tests that all protocols have a default implementation for java.lang.Object
;; (except for specified known exceptions
(deftest test-default-implementations
  (is (= #{'PIndexedSettingMutable 'PMatrixRank 'PGenericOperations 'PDatasetImplementation}
         (set (map :name (filter #(not (extends? % java.lang.Object)) (extract-protocols)))))))
