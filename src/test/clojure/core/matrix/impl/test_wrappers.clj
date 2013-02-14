(ns core.matrix.impl.test-wrappers
  (:use clojure.test)
  (:use core.matrix)
  (:use core.matrix.impl.wrappers) 
  (:require [core.matrix.operators :as op]) 
  (:require [core.matrix.compliance-tester]))

(deftest assumptions
  (is (== 0 (dimensionality (wrap-nd 7))))
  (is (== 1 (dimensionality (wrap-nd [7]))))
  (is (not (scalar? (wrap-nd 7))))
  (is (equals 7 (wrap-nd 7))))

(deftest test-wrapped-slice
  (let [m (wrap-slice [[1 2] [3 4]] 1)]
    (equals [4 5] (emap inc m))))

(deftest test-wrapped-nd
  (let [m (wrap-nd [3 4])]
    (equals [4 5] (emap inc m))))

(deftest test-slice-wrap
  (is (equals [3 4] (wrap-slice [[1 2] [3 4]] 1)))
  (is (equals 3 (wrap-slice [3 4] 0))))

(deftest test-nd-wrap
  (is (equals [3 4] (wrap-nd [3 4]))))

(deftest instance-tests
  (core.matrix.compliance-tester/instance-test (wrap-slice [[1 2] [3 4]] 1))
  (core.matrix.compliance-tester/instance-test (wrap-nd [[1 2] [3 4]]))) 
