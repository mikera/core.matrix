(ns core.matrix.compliance-tester
  (:use core.matrix)
  (:use clojure.test)
  (:require [core.matrix.implementations :as imp])
  (:use core.matrix.utils))

;; ====================================
;; COMPLIANCE TESTING
;;
;; test suite that implementations can call to test
;; adherence to core.matrix API contracts
;;
;; Note that tests need to be written in a very generic way
;; - they can't assume anything other than documented API behaviour!
;; 
;; e.g. can't assume that scalar values are always Doubles etc.

;; ===========================================
;; Utility functions

(defn mutable-equivalent? 
  [m mutable-fn immutable-fn]
  (or
    (not (mutable? m))
    (let [clonem (clone m)]
      (mutable-fn clonem)
      (equals clonem (immutable-fn m)))))

;; ===========================================
;; General implementation tests

(defn test-implementation-key
  [m]
  (testing "Implementation keyword"
    (is (keyword? (imp/get-implementation-key m)))
    (is (= (imp/get-implementation-key m) (imp/get-implementation-key (imp/get-canonical-object m))))))


;; TODO: figure out what to do with implementations that only support specific types?
(defn test-new-matrices [m]
  (testing "Vector construction"
    (when (supports-dimensionality? m 1)
      (let [v (matrix [1])]
        (is (== 1.0 (mget v 0))))))
  (testing "Matrix construction"
    (when (supports-dimensionality? m 2)
      (let [m (matrix [[1 2] [3 4]])]
        (is (== 3.0 (mget m 1 0)))))))

(defn test-coerce-via-vectors [m]
  (testing "Vector coercion"
    (let [v (matrix [1])]
      (is (equals [1] (to-nested-vectors v)))))
  (testing "Matrix coercion"
    (when (supports-dimensionality? m 2)
      (let [m (matrix [[1 2] [3 4]])]
        (is (equals [[1 2] [3 4]] (to-nested-vectors m)))))))

(defn test-dimensionality [m]
  (testing "shape"
    (is (>= (count (shape m)) 0))
    (is (= (seq (shape m)) (for [i (range (dimensionality m))] (dimension-count m i)))))
  (testing "element count"
    (is (== (ecount m) (reduce * 1 (shape m))))))

;; ========================================
;; numeric function tests

(defn test-scale
  ([m]
    (is (mutable-equivalent? m #(scale! % 2) #(scale % 2)))))

(defn test-numeric-functions [m]
  (when (supports-dimensionality? m 2)
    (let [m (matrix m [[1 2] [3 4]])]
      (test-scale m)))
  (when (supports-dimensionality? m 1)
    (let [m (matrix m [1 2 3])]
      (test-scale m))))


;; ========================================
;; 2D tests

(defn test-transpose [m]
  (testing "2D transpose"
    (let [m (matrix [[1 2] [3 4]])]
      (is (equals [[1 3] [2 4]] (transpose m))))))

(defn test-identity [m]
  (let [I (identity-matrix m 3)]
    (is (equals [1 2 3] (mul I [1 2 3])))))


(defn test-diagonal [m]
  (let [I (diagonal-matrix m [1 2 3])]
    (is (equals [1 4 9] (mul I [1 2 3])))))

(defn matrix-tests-2d [m]
  (test-transpose m)
  (test-identity m))

;; ======================================
;; Main compliance test method
;; 
;; implementations should call this with either a valid instance or their registered implementation key
(defn compliance-test 
  "Runs the compliance test suite on a given matrix implementation. 
   m can be either a matrix instance or the implementation keyword."
  [m]
  (let [m (imp/get-canonical-object m)
        ik (imp/get-implementation-key m)]
    (binding [*matrix-implementation* ik]
      (test-implementation-key m)
      (test-coerce-via-vectors m)
      (when (supports-dimensionality? m 2)
        (matrix-tests-2d m))
      (test-numeric-functions m)
      (test-dimensionality m)
      (test-new-matrices m))))