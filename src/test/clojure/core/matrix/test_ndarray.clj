(ns core.matrix.test-ndarray
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require core.matrix.impl.persistent-vector)
  (:use core.matrix.impl.ndarray))

(deftest test-ndarray-base
  (testing "construction"
    (is (= [3 3] (dimensions (make-ndarray [3 3])))))
  (testing "getters"
    (is (= nil (mget (make-ndarray [3 3]) 2 2)))
    (is (= nil (mget (make-ndarray [3 3 3]) 1 1 1)))))

