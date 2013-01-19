(ns core.matrix.test-persistent-vector-implementation
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require [core.matrix.compliance-tester])
  (:require core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

(deftest test-indexed-access
  (testing "indexed access to java.util.List"
    (let [al (java.util.ArrayList.)]
      (.add al 1.0)
      (.add al 2.0)
      (.add al 3.0)
      (is (= [1.0 2.0 3.0] (coerce [] al)))
      (is (== 1.0 (mget al 0)))))
  
  (testing "trace"
    (is (== 5 (trace [[1 2] [3 4]])))))

(deftest test-transpose
  (testing "vector transpose"
    (= [[1 3] [2 3]] (transpose [[1 2] [3 4]]))
    (= [[1 2 3]] (transpose [1 2 3]))))

(deftest test-functional-op
  (testing "map"
    (is (= [1 2] (emap inc [0 1])))
    (is (= [1 2] (emap + [0 1] [1 1])))
    (is (= [3 5] (emap + [0 1] [1 1] [0 0] [2 3])))
    (is (= [[2.0 0.0] [0.0 2.0]] (emap #(* 2 %) [[1.0 0.0] [0.0 1.0]])))))

(deftest test-matrix-multiply
  (testing "matrix multiplication"
    (is (= [[5.0 10.0] [15.0 20.0]] (mul [[1 2] [3 4]] 5)))
    (is (= [[1 0] [2 2] [5 0]] (mul [[1 0] [0 2] [5 0]] [[1 0] [1 1]])))
    (is (= [[1 2] [3 4]] (mul [[1 2] [3 4]] [[1 0] [0 1]])))
    (is (= [[5]] (mul [[1 2]] [[1] [2]])))))

(deftest test-transform
  (testing "matrix transform"
    (is (= [5 10] (transform [[1 0] [0 2]] [5 5]))))
  (testing "function transform"
    (is (= [1 2] (transform (fn [_] [1 2]) [5 5])))))

;; run complicance tests

(deftest compliance-test
  (core.matrix.compliance-tester/compliance-test [])) 
