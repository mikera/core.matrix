(ns clojure.core.matrix.test-double-array
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.double-array))

;; This namespace contains tests for the Java double[] array implementation
;;
;; This is an important implementation, as it provides maximum efficiency for many
;; numerical vector operations with mutable vectors

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

(deftest row-setting
  (let [a (double-array [1 2 3])]
    (is (equals [1 10 3] (set-row a 1 10)))
    (set-row! a 0 7)
    (is (equals [7 2 3] (seq a)))))

(deftest column-setting
  (let [a [(double-array [1 2]) (double-array [3 4])]]
    (is (equals [[1.0 10.0] [3.0 10.0]] (set-column a 1 10)))))

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
      (is (scalar? fs))
      (is (== 0 (dimensionality fs)))
      (is (not (array? fs)))))
  (testing "slice views"
    (let [da (double-array [1 2 3])
          fs (first (slice-views da))]
      (is (not (scalar? fs)))
      (is (== 0 (dimensionality fs)))
      (fill! fs 10) 
      (is (equals [10 2 3] da))
      (is (array? fs))))
  (testing "wrong dimension"
    (let [da (double-array [1 2 3])]
      (is (error? (slice da 1 1))))))

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

(deftest test-add-scaled
  (let [da (double-array [1 2])]
    (is (equals [11 22] (add-scaled da [1 2] 10)))
    (is (equals [101 202] (add-scaled! da [1 2] 100)))
    (is (equals [101 202] da))))

(deftest test-add-scaled-product
  (let [da (double-array [1 2])]
    (is (equals [2 6] (add-scaled-product da [1 2] [1 2] 1)))
    (is (equals [101 202] (add-scaled-product! da [1 2] [10 10] 10)))
    (is (equals [101 202] da))))

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

(deftest test-mutable-map!
  (let [v (double-array [1 2 3])]
    (emap! clojure.core/+ v [10 10 10])
    (is (equals v [11 12 13]))))

(deftest test-div
  (is (equals [1 2] (div (double-array [2 4]) 2))))

(deftest test-div!
  (let [da (double-array [2 4])]
    (div! da 2)
    (is (equals [1 2] da))))

(deftest test-broadcast-coerce
  (is (= [1.0 2.0] (mp/broadcast-coerce [0 0] (double-array [1 2])))))

(deftest test-mutable-multiply
  (let [a (double-array [1 2])
        b (double-array [2 3])]
    (is (identical? a (emul! a b)))
    (is (equals [2.0 6.0] (vec a)))
    (is (equals [2.0 6.0] a))))

(deftest test-element-ops
  (let [a (double-array [5 1 7 8 4])]
    (is (== 1 (emin a)))
    (is (== 8 (emax a)))
    (is (== 25 (esum a)))))

(deftest test-maths-ops
  (testing "basic ops"
    (let [da (double-array [1.2 2.3])]
      (is (equals [1.0 2.0] (floor da))))))

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (double-array []))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [1]))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [1 2]))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [-1 4 2 7 -3])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (double-array [0.23])))
