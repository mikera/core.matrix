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

(defn create-dimensioned 
  "Create a test nested vector array with the specified number of dims. will have 2^dims numeric elements"
  ([dims]
    (create-dimensioned dims 1))
  ([dims start]
    (cond 
      (<= dims 0) start
      :else (vector (create-dimensioned (dec dims) start)
                    (create-dimensioned (dec dims) (+ start (bit-shift-left 1 (dec dims))))))))

(defn create-supported-matrices
  "Creates a set of vector matrices of supported dimensionalities from 1 to 4"
  ([m]
    (map 
      #(create-dimensioned %)
      (filter #(supports-dimensionality? m %)
              (range 1 5)))))

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
        (is (== 3.0 (mget m 1 0))))))
  (testing "All supported sizes")
    (doseq [vm (create-supported-matrices m)]
      (let [m (matrix vm)]
        (equals vm m))))

(defn test-coerce-via-vectors [m]
  (testing "Vector coercion"
    (when (supports-dimensionality? m 1)
      (let [v (matrix [1])]
        (is (equals [1] (to-nested-vectors v))))))
  (testing "Matrix coercion"
    (when (supports-dimensionality? m 2)
      (let [m (matrix [[1 2] [3 4]])]
        (is (equals [[1 2] [3 4]] (to-nested-vectors m)))))))

(defn test-dimensionality [m]
  (testing "shape"
    (is (>= (count (shape m)) 0))
    (is (= (seq (shape m)) (for [i (range (dimensionality m))] (dimension-count m i)))))
  (testing "element count"
    (is (== (ecount m) (reduce * 1 (shape m)))))
  (testing "supported matrix size tests"
    (doseq [vm (create-supported-matrices m)]
      (let [m (coerce m vm)]
        (is (= (seq (shape m)) (seq (shape vm))))
        (is (= (ecount m) (ecount vm)))))))

;; =======================================
;; array interop tests

(defn test-array-assignment
  [m]
  (when (mutable? m)
    (doseq [vm (create-supported-matrices m)]
      (let [m (coerce m vm)
            len (ecount m)
            vs (range 1 (inc len))
            arr (into-array vs)]
        (is (= vs (eseq m)))
        (scale! m 0.0)
        (is (== 0.0 (first (eseq m))))
        (assign! m arr)
        (is (= vs (eseq m)))))))

(defn test-array-interop [m]
  (test-array-assignment [m]))

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
      (test-array-interop m)
      (test-numeric-functions m)
      (test-dimensionality m)
      (test-new-matrices m))))