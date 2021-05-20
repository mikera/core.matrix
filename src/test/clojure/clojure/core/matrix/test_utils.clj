(ns clojure.core.matrix.test-utils
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [extract-protocols]]
            [clojure.test :refer :all])
  (:import [clojure.lang PersistentVector]
           [mikera.vectorz Vector]))

;; Tests for clojure.core.matrix.utils functions and macros

(deftest test-long-array
  (is (= (type (long-array 0)) (type (long-array nil))))
  (is (= (count (long-array 0)) (count (long-array nil)))))

;; this tests that all protocols have a default implementation for java.lang.Object
;; (except for specified known exceptions
(deftest test-default-implementations
  (is (= #{'PIndexedSettingMutable 'PMatrixRank 'PGenericOperations 'PDatasetImplementation}
         (set (map :name (filter #(not (extends? % Object)) (extract-protocols)))))))
