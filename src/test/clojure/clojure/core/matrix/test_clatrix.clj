(ns clojure.core.matrix.test-clatrix
  (:require [clojure.core.matrix.impl.pprint :as pprint]
            [clatrix.core :as clatrix]
            [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]))

;; Tests for the Vectorz implementation, an important high speed JVM array library
;;
;; Not intended to be exhasustive (since vectorz-clj has its own tests) however
;; this is useful to detect breaking changes early.

(deftest compliance-tests-1D
  (compliance/instance-test (array :clatrix [1 2 3])))

(deftest compliance-tests-2D
  (compliance/instance-test (array :clatrix [[1 2] [3 4]])))

;; TODO: consider what should happen for 3D arrays in Clatrix?
;(deftest compliance-tests-3D
;  (compliance/instance-test (array :clatrix [[[1 2] [3 4]] [[5 6] [7 8]]])))