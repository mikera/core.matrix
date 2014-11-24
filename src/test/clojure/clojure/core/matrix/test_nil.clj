(ns clojure.core.matrix.test-nil
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [error?]]
            [clojure.test :refer :all]))

;; Tests for the specific behaviour of core.matrix functions on the nil value (a scalar)

(deftest test-scalar-properties
  (is (not (array? nil)))
  (is (nil? (shape nil)))
  (is (== 1 (ecount nil)))
  (is (not (e= nil [])))
  (is (not (e= nil '())))
  (is (e= nil nil))
  (is (nil? (assign 1 nil)))
  (is (nil? (ereduce + nil))))

(deftest test-nil
  (is (nil? (transpose nil)))
  (is (== 0 (dimensionality nil)))
  (is (nil? (shape nil))))

(deftest test-arithmentic
  (is (error? (add nil 1))))

(deftest test-broadcast
  (is (e= [nil nil] (broadcast nil [2])))
  (is (e= [nil nil] (assign [1 2] nil))))

(deftest test-join
  (is (e= [nil nil] (join [nil] [nil]))))

(deftest test-set
  (is (equals 3 (mset nil 3)))
  (is (error? (mset! nil 3)))
  (is (error? (mset nil 2 3))))

(deftest instance-tests
  (compliance/instance-test nil))
