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

(defn test-implementation-key
  [m]
  (testing "Implementation keyword"
    (is (keyword? (imp/get-implementation-key m)))
    (is (= (imp/get-implementation-key m) (imp/get-implementation-key (imp/get-canonical-object m))))))


;; TODO: figure out what to do with implementations that only support specific types?
(defn test-new-matrices [m]
  (testing "Vector construction"
    (let [v (matrix [1])]
      (is (== 1.0 (mget v 0)))))
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
    (is (= (seq (shape m)) (for [i (range (dimensionality m))] (dimension-count m i))))))


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
      (test-dimensionality m)
      (test-new-matrices m))))