(ns clojure.core.matrix.test-nil
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :as m])
  #?(:clj (:require [clojure.core.matrix.macros-clj :refer [error?]]
                     [clojure.test :refer [deftest is testing run-tests]])
     :cljs (:require-macros
                   [clojure.core.matrix.macros-cljs :refer [error?]]
                   [cljs.test :refer [deftest is testing run-tests]])))

;; Tests for the specific behaviour of core.matrix functions on the nil value (a scalar)

(deftest test-scalar-properties
  (is (not (m/array? nil)))
  (is (nil? (m/shape nil)))
  (is (== 1 (m/ecount nil)))
  (is (not (m/e= nil [])))
  (is (not (m/e= nil '())))
  (is (m/e= nil nil))
  (is (nil? (m/assign 1 nil)))
  (is (nil? (m/ereduce + nil))))

(deftest test-nil
  (is (nil? (m/transpose nil)))
  (is (== 0 (m/dimensionality nil)))
  (is (nil? (m/shape nil))))

(deftest test-arithmentic
  (is (error? (m/add nil 1))))

(deftest test-broadcast
  (is (m/e= [nil nil] (m/broadcast nil [2])))
  (is (m/e= [nil nil] (m/assign [1 2] nil))))

(deftest test-join
  (is (m/e= [nil nil] (m/join [nil] [nil]))))

(deftest test-set
  (is (m/equals 3 (m/mset nil 3)))
  (is (error? (m/mset! nil 3)))
  (is (error? (m/mset nil 2 3))))

(deftest instance-tests
  (compliance/instance-test nil))
