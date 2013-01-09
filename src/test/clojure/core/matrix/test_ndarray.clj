(ns core.matrix.test-ndarray
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require core.matrix.impl.persistent-vector)
  (:use core.matrix.impl.ndarray))

(deftest test-ndarray-base
  (testing "construction"
    (= [3 3] (all-dimensions (make-ndarray [3 3])))))
