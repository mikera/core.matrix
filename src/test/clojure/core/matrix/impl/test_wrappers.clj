(ns core.matrix.impl.test-wrappers
  (:use clojure.test)
  (:use core.matrix)
  (:use core.matrix.impl.wrappers) 
  (:require [core.matrix.operators :as op]) 
  (:require [core.matrix.compliance-tester]))

(deftest test-slice-wrap
  (is (equals [3 4] (wrap-slice [[1 2] [3 4]] 1))))

(deftest compliance-test
  (core.matrix.compliance-tester/instance-test (wrap-slice [[1 2] [3 4]] 1))) 
