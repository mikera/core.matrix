(ns clojure.core.matrix.test-object-array
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.double-array))

;; Tests for core.matrix functions on Java Object [] arrays
;;
;; This is an important implementation because it provides efficient support
;; for mutable vectors of arbitrary objects on the JVM

(deftest regressions
  (is (= [2] (seq (emap inc (object-array [1]))))))

(deftest to-objects
  (is (equals [0 1 2] (to-object-array (range 3))))
  (is (e= [1 2 3 :foo] (to-object-array [[1 2] [3 :foo]]))))

(deftest instance-tests
  ;(clojure.core.matrix.compliance-tester/instance-test (object-array []))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [1]))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [1 :foo]))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [-1 4 2 7 -3])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (object-array [0.23])))
