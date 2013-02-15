(ns clojure.core.matrix.compliance-tester
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.core.matrix.implementations :as imp])
  (:use clojure.core.matrix.utils))

;; ====================================
;; COMPLIANCE TESTING
;;
;; test suite that implementations can call to test
;; adherence to clojure.core.matrix API contracts
;;
;; Note that tests need to be written in a very generic way
;; - they can't assume anything other than documented API behaviour!
;; 
;; e.g. we can't assume that scalar values are always Doubles etc.

;; ===========================================
;; Utility functions

(defn mutable-equivalent? 
  "Returns true if mutable-fn? is the in-place equivalent of immutable-fn? when applied to m"
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
  [im]
  (testing "Implementation keyword"
    (is (keyword? (imp/get-implementation-key im)))
    (is (= (imp/get-implementation-key im) (imp/get-implementation-key (imp/get-canonical-object im)))))
  (testing "Implementation building same type"
    (is (= (imp/get-implementation-key im) (imp/get-implementation-key (matrix im))))
    (is (= (imp/get-implementation-key im) (imp/get-implementation-key (matrix im im))))))

(defn test-implementation [im]
  (test-implementation-key im))

;; ==============================================
;; Test general array assumprions
;;
;; should work on any array with Number elements

(defn test-double-array-ops [m]
  (let [aa (to-double-array m true)  ;; a new copy
        ta (to-double-array m)       ;; an array, may or may not be a new copy
        ca (to-double-array m false) ;; not a copy, i.e. internal array or nil
        c (ecount m)]
    (is (= c (count aa) (count ta)))
    (when ca
      (is (= c (count ca)))
      (is (= (seq ta) (seq ca))))
    (is (= (seq ta) (map double (eseq m))))))

(defn test-dimensionality-assumptions [m]
  (testing "shape"
    (is (>= (count (shape m)) 0))
    (is (= (seq (shape m)) (for [i (range (dimensionality m))] (dimension-count m i)))))
  (testing "element count"
    (is (== (ecount m) (reduce * 1 (shape m))))))

(defn test-reshape [m]
  (let [c (ecount m)]
    (when (supports-dimensionality? m 1) 
      (= (eseq m) (eseq (reshape m [c]))))
    (when (supports-dimensionality? m 2)
      (= (eseq m) (eseq (reshape m [1 c])))
      (= (eseq m) (eseq (reshape m [c 1]))))))

(defn test-vector-round-trip [m]
  (is (equals m (coerce m (coerce [] m)))))

(defn test-array-assumptions [m]
  (test-double-array-ops m)
  (test-reshape m)
  (test-dimensionality-assumptions m)
  (test-vector-round-trip m))

(defn test-assumptions-for-all-sizes [im]
  (doseq [vm (create-supported-matrices im)]
    (let [m (matrix im vm)]
      (test-array-assumptions m))))

;; ==============================================
;; misc tests

;; TODO: figure out what to do with implementations that only support specific types?
(defn test-new-matrices [im]
  (testing "Vector construction"
    (when (supports-dimensionality? im 1)
      (let [v (matrix im [1])]
        (is (== 1.0 (mget v 0))))))
  (testing "Matrix construction"
    (when (supports-dimensionality? im 2)
      (let [m (matrix im [[1 2] [3 4]])]
        (is (== 3.0 (mget m 1 0))))))
  (testing "All supported sizes")
    (doseq [vm (create-supported-matrices im)]
      (let [m (matrix im vm)]
        (equals vm m))))

(defn test-coerce-via-vectors [m]
  (testing "Vector coercion"
    (when (supports-dimensionality? m 1)
      (testing "coerce works"
        (or (= (imp/get-implementation-key m) (imp/get-implementation-key (coerce m [1])))))
      (let [v (matrix [1])]
        (is (equals [1] (to-nested-vectors v))))))
  (testing "Matrix coercion"
    (when (supports-dimensionality? m 2)
      (testing "coerce works"
        (or (= (imp/get-implementation-key m) (imp/get-implementation-key (coerce m [[1 2] [3 4]])))))
      (let [m (matrix [[1 2] [3 4]])]
        (is (equals [[1 2] [3 4]] (to-nested-vectors m)))))))

(defn test-dimensionality [im]
  (testing "supported matrix size tests"
    (doseq [vm (create-supported-matrices im)]
      (let [m (coerce im vm)]
        (is (= (seq (shape m)) (seq (shape vm))))
        (is (= (ecount m) (ecount vm)))
        (is (= (eseq m) (eseq (emap identity m))))))))


;; =======================================
;; array interop tests

(defn test-array-assignment
  [im]
  (when (mutable? im)
    (doseq [vm (create-supported-matrices im)]
      (let [m (coerce im vm)
            len (ecount m)
            vs (range 1 (inc len))
            arr (into-array vs)]
        (is (= len (count vs)))
        (is (every? true? (map == vs (eseq m))))
        (when-not (mutable? m) 
          (error "Problem: coerced object not mutable?"))
        (scale! m 0.0)
        (is (== 0.0 (first (eseq m))))
        (assign! m arr)
        (is (every? true? (map == vs (eseq m))))))))

(defn test-array-interop [im]
  (test-array-assignment im))

;; ========================================
;; numeric function tests

(defn test-scale
  ([m]
    (is (mutable-equivalent? m #(scale! % 2) #(scale % 2)))))

(defn test-numeric-functions [im]
  (when (supports-dimensionality? im 2)
    (let [m (matrix im [[1 2] [3 4]])]
      (is (== 10 (sum m)))
      (test-scale m)))
  (when (supports-dimensionality? im 1)
    (let [m (matrix im [1 2 3])]
      (is (== 6 (sum m)))
      (test-scale m))))

;; ========================================
;; 1D vector tests

(defn test-vector-slices [im]
  (let [m (matrix im [1 2 3])]
    (is (equals m (matrix im (vec (slices m)))))
    (is (= (slices m) (eseq m)))))

(defn test-element-add [im]
  (is (equals [2.0 4.0] (emap + (matrix im [1 3]) (coerce im [1 1]))))
  (is (equals [2.0 4.0] (emap inc (matrix im [1 3])))))

(defn vector-tests-1d [im]
  (test-vector-slices im)
  (test-element-add im))

;; ========================================
;; 2D matrix tests

(defn test-transpose [im]
  (testing "2D transpose"
    (let [im (matrix [[1 2] [3 4]])]
      (is (equals [[1 3] [2 4]] (transpose im)))
      (is (equals im (transpose (transpose im)))))))

(defn test-identity [im]
  (let [I (identity-matrix im 3)]
    (is (equals [1 2 3] (mul I [1 2 3])))
    (is (equals I (transpose I)))))


(defn test-diagonal [im]
  (let [I (diagonal-matrix im [1 2 3])]
    (is (equals [1 4 9] (mul I [1 2 3])))
    (is (equals I (transpose I)))))

(defn test-row-column-matrices [im]
  (let [rm (row-matrix im [1 2 3])]
    (is (= [1 3] (shape rm)))
    (is (equals [[1 2 3]] rm))
    (is (row-matrix? rm))
    (is (column-matrix? (transpose rm))))
  (let [cm (column-matrix im [1 2 3])]
    (is (= [3 1] (shape cm)))
    (is (equals [[1] [2] [3]] cm))
    (is (column-matrix? cm))
    (is (row-matrix? (transpose cm)))))

(defn matrix-tests-2d [im]
  (test-row-column-matrices im)
  (test-transpose im)
  (test-diagonal im)
  (test-identity im))

;; ======================================
;; Instance test function
;;
;; Implementations can call to test specific instances of interest
(defn instance-test [m]
  (test-array-assumptions [m]))

;; ======================================
;; Main compliance test method
;; 
;; implementations should call this with either a valid instance or their registered implementation key
;;
;; Convention: im refers to the origincal matrix implementatin object
(defn compliance-test 
  "Runs the compliance test suite on a given matrix implementation. 
   m can be either a matrix instance or the implementation keyword."
  [m]
  (let [im (imp/get-canonical-object m)
        ik (imp/get-implementation-key im)]
    (binding [*matrix-implementation* ik]
      (test-implementation im)
      (test-assumptions-for-all-sizes im)
      (test-coerce-via-vectors im)
      (when (supports-dimensionality? im 2)
        (matrix-tests-2d im))
      (when (supports-dimensionality? im 1)
        (vector-tests-1d im))
      (test-array-interop im)
      (test-numeric-functions im)
      (test-dimensionality im)
      (test-new-matrices im))))