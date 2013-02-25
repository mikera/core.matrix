(ns clojure.core.matrix.test-double-array
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.double-array))

(deftest misc-regressions
  (testing "shape sequnces"
    (is (= [0] (shape (double-array []))))
    (is (= [1 0] (shape [(double-array [])])))
    (is (= [0] (shape (transpose (double-array [])))))))

(deftest test-create
  (testing "making a double array"
    (let [da (matrix :double-array [1 2])]
      (is (= [1.0 2.0] (seq da)))
      (is (= [1.0 2.0] (eseq da)))
      (is (= (class (double-array [1])) (class da)))))
  (testing "coercion from persistent vector"
    (let [da (matrix :double-array [1 2])]
      (is (= [2.0 4.0] (seq (coerce da [2 4]))))
      (is (= (class da) (class (coerce da [2 4])))))))

(deftest test-type
  (is (= Double/TYPE (element-type (double-array [1 2])))))

(deftest test-slices
  (testing "slices"
    (let [m [(double-array [1 2]) (double-array [3 4])]]
      (is (equals [1 2] (get-row m 0)))
      (is (equals [2 4] (get-column m 1)))
      (is (e= [1.0 2.0] (slices (get-row m 0))))
      (is (e= [2.0 4.0] (slices (get-column m 1)))))))

(deftest test-scalar-slices
  (testing "slices"
    (let [da (double-array [1 2 3])
          fs (first (slices da))]
      (is (not (scalar? fs)))
      (is (== 0 (dimensionality fs)))
      (is (array? fs)))))

(deftest test-functional-ops
  (testing "mapping"
    (let [da (matrix :double-array [1 2])]
      (is (= [2.0 3.0] (seq (emap inc da))))
      (emap! inc da) 
      (is (= [2.0 3.0] (eseq da)))))
  (testing "nested double arrays"
    (is (= [1.0 2.0 3.0 4.0] (eseq [(double-array [1 2]) (double-array [3 4])])))))

(deftest test-assign
  (testing "assign from a persistent vector"
    (let [da (double-array [1 2])]
      (assign! da [2 3]) 
      (is (= [2.0 3.0] (seq da)))))
  (testing "assign from an array"
    (let [da (double-array [1 2])]
      (assign! da (double-array [2 4])) 
      (is (= [2.0 4.0] (seq da)))))
  (testing "assign from a Number array"
    (let [da (double-array [1 2])]
      (mp/assign-array! da (into-array java.lang.Number [2 5])) 
      (is (= [2.0 5.0] (seq da))))))

(deftest test-equals
  (testing "equality with persistent vector"
    (let [da (double-array [1 2])] 
      (is (= [1.0 2.0] (to-nested-vectors da)))
      (is (equals [1.0 2.0] da))
      (is (equals [1 2] da))
      (is (equals da [1.0 2.0])))))

(deftest test-vector-scale
  (testing "scale!"
    (let [da (double-array [1.0 2.0])]
      (is (equals [2.0 4.0] (scale! da 2))))))

(deftest test-vector-normalise
  (testing "normalise!"
    (let [da (double-array [4.0])]
      (is (equals [1.0] (normalise! da))))))

(deftest test-mutable-add
  (let [v (double-array [1 2 3])]
    (add! v [10 10 10])
    (sub! v [1 1 2])
    (is (equals v [10 11 11])))) 

(deftest test-maths-ops
  (testing "basic ops"
    (let [da (double-array [1.2 2.3])]
      (is (equals [1.0 2.0] (floor da))))))

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (double-array []))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [1 2]))) 

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (double-array [0.23]))) 
