(ns clojure.core.matrix.impl.test-wrappers
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.impl.wrappers) 
  (:require [clojure.core.matrix.operators :as op]) 
  (:require [clojure.core.matrix.compliance-tester]))

(deftest test-slice-wrap
  (is (equals [3 4] (wrap-slice [[1 2] [3 4]] 1))))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/instance-test (wrap-slice [[1 2] [3 4]] 1))) 
