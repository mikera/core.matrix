(ns clojure.core.matrix.compliance-tester
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.core.matrix.protocols :as mp])
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
;;
;; Convention: im refers to the original matrix implementation object
;;             m refers to a specific matrix instance to be tested

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
    (is (= (seq (shape m)) 
           (seq (for [i (range (dimensionality m))] (dimension-count m i))))))
  (testing "vectors always have dimensionality == 1"
    (is (or (= (boolean (vec? m)) (boolean (== 1 (dimensionality m)))) (error "Failed with : " m))))
  (testing "scalars always have dimensionality == 0"
    (is (or (not (scalar? m)) (== 0 (dimensionality m)))))
  (testing "zero-dimensionality"
    (is (= (zero-dimensional? m) (== 0 (dimensionality m)))))
  (testing "element count"
    (is (== (ecount m) (reduce * 1 (shape m))))
    (is (== (ecount m) (ereduce (fn [acc _] (inc acc)) 0 (eseq m))))
    (is (or (not (scalar? m)) (== 1 (ecount m))))))

(defn test-mutable-assumptions [m]
  (testing "modifying a cloned mutable array does not modify the original" 
    (when (and (mutable? m) (> 0 (ecount m)))
	    (let [cm (clone m)
	          ix (first (index-seq m))
	          v1 (first (eseq m))
	          v2 (if (and (number? v1) (== v1 0)) 1 0)]
	      (apply mset! cm (concat ix [v2]))
	      (is (equals v1 (apply mget m ix)))
	      (is (equals v2 (apply mget cm ix))))))
  (testing "attempt to modify an immutable array results in an exception"
    (when (not (mutable? m))
	    (let [cm (clone m)
	          ix (first (index-seq m))
	          v1 (first (eseq m))
	          v2 (if (and (number? v1) (== v1 0)) 1 0)]
	      (is (thrown? Throwable (apply mset! cm (concat ix [v2]))))))))

(defn test-reshape [m]
  (let [c (ecount m)]
    (when (> c 0)
      (when (supports-dimensionality? m 1) 
        (= (eseq m) (eseq (reshape m [c]))))
      (when (supports-dimensionality? m 2)
        (= (eseq m) (eseq (reshape m [1 c])))
        (= (eseq m) (eseq (reshape m [c 1])))))))

(defn test-broadcast [m]
  (let [c (ecount m)]
    (when (> c 0)
      (e= m (first (slices (broadcast m (cons 2 (shape m))))))
      (== c (ecount (broadcast m (cons 1 (shape m)))))
      (== (* 3 c) (ecount (broadcast m (cons 3 (shape m))))))))

(defn test-slice-assumptions [m]
  (let [dims (dimensionality m)]
    (when (> dims 0)
      (doseq [sl (slices m)]
        (is (== (dec dims) (dimensionality sl)))
        (is (= (next (shape m)) (seq (shape sl)))))
      (if-let [ss (seq (slices m))]
        (let [fss (first ss)]
          (is (= (mutable? fss) (mutable? m))))))))

(defn test-general-transpose [m]
  (when (> (ecount m) 0) 
    (let [mt (transpose m)]
      (is (e= m (transpose mt)))
      (is (= (seq (shape m)) 
             (seq (reverse (shape mt))))))))

(defn test-coerce [m]
  (let [vm (mp/convert-to-nested-vectors m)]
      (is (clojure.core.matrix.impl.persistent-vector/is-nested-vectors? vm))
      (is (e= m vm))))

(defn test-vector-round-trip [m]
  (is (e= m (coerce m (coerce [] m)))))

(defn test-array-assumptions [m]
  ;; note: these must work on *any* array, i.e. no pre-assumptions on element type etc.
  (test-coerce m)
  (test-dimensionality-assumptions m)
  (test-slice-assumptions m)
  (test-mutable-assumptions m)
  (test-vector-round-trip m)
  (test-reshape m)
  (test-broadcast m)
  (test-general-transpose m))

(defn test-assumptions-for-all-sizes [im]
  (doseq [vm (create-supported-matrices im)]
    (let [m (matrix im vm)]
      (test-array-assumptions m)
      (test-double-array-ops m))))



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
        (e== vm m))))

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
        (assign-array! m arr)
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
      (is (== 10 (esum m)))
      (test-scale m)))
  (when (supports-dimensionality? im 1)
    (let [m (matrix im [1 2 3])]
      (is (== 6 (esum m)))
      (test-scale m))))

;; ========================================
;; 1D vector tests

(defn test-vector-slices [im]
  (let [m (matrix im [1 2 3])]
    (is (equals m (matrix im (coerce [] (slices m)))))
    (is (= (map mp/get-0d (slices m)) (eseq m)))))

(defn test-vector-subvector [im]
  (let [m (matrix im [1 2 3])]
    (is (equals [2] (subvector m 1 1)))
    (is (equals m (subvector m 0 3)))))

(defn test-element-add [im]
  (is (equals [2.0 4.0] (emap + (matrix im [1 3]) (coerce im [1 1]))))
  (is (equals [2.0 4.0] (emap inc (matrix im [1 3])))))

(defn test-vector-mset [im]
  (let [m (matrix im [1 2 3])]
    (is (equals [1 2 4] (mset m 2 4)))
    (is (equals [1 2 3] m))))

(defn test-vector-cross [im]
  (let [m (matrix im [1 2 3])]
    (is (equals [0 0 0] (cross m m)))
    (is (equals [-1 2 -1] (cross m [1 1 1])))
    (is (mutable-equivalent? m #(cross! % [3 4 5]) #(cross % [3 4 5])))))

(defn test-vector-mutable-add [im]
  (let [m (matrix im [1 2 3])]
    (is (mutable-equivalent? m #(add! % [3 4 5]) #(add % [3 4 5])))
    (is (mutable-equivalent? m #(sub! % [3 4 5]) #(sub % [3 4 5])))))

(defn test-vector-length [im]
  (let [m (matrix im [3 4])]
    (is (== 5 (length m)))
    (is (== 25 (dot m m)))))

(defn test-vector-normalise [im]
  (let [m (matrix im [3 4])
        n (normalise m)]
    (is (equals n [0.6 0.8] 0.000001))
    (is (mutable-equivalent? m normalise! normalise)))) 

(defn vector-tests-1d [im]
  (test-vector-mset im)
  (test-vector-length im)
  (test-vector-cross im)
  (test-vector-mutable-add im)
  (test-vector-normalise im)
  (test-vector-slices im)
  (test-vector-subvector im)
  (test-element-add im))

;; ========================================
;; 2D matrix tests

(defn test-transpose [im]
  (testing "2D transpose"
    (let [im (matrix [[1 2] [3 4]])]
      (is (equals [[1 3] [2 4]] (transpose im)))
      (is (equals im (transpose (transpose im)))))))

(defn test-negate [im]
  (testing "negate"
    (let [m (matrix [[1 2] [3 4]])]
      (is (equals [[-1 -2] [-3 -4]] (negate m))))))

(defn test-identity [im]
  (let [I (identity-matrix im 3)]
    (is (equals [1 2 3] (mul I [1 2 3])))
    (is (equals I (transpose I)))))

(defn test-trace [im]
  (let [I (identity-matrix im 3)]
    (is (== 3.0 (trace I))))
  (let [m (matrix im [[1 2] [3 4]])]
    (is (== 5.0 (trace m)))))

(defn test-diagonal [im]
  (let [I (diagonal-matrix im [1 2 3])]
    (is (equals [1 4 9] (mul I [1 2 3])))
    (is (equals I (transpose I)))))

(defn test-row-column-matrices [im]
  (let [rm (row-matrix im [1 2 3])]
    (is (= [1 3] (seq (shape rm))))
    (is (equals [[1 2 3]] rm))
    (is (row-matrix? rm))
    (is (column-matrix? (transpose rm))))
  (let [cm (column-matrix im [1 2 3])]
    (is (= [3 1] (seq (shape cm))))
    (is (equals [[1] [2] [3]] cm))
    (is (column-matrix? cm))
    (is (row-matrix? (transpose cm)))))

(defn matrix-tests-2d [im]
  (test-row-column-matrices im)
  (test-transpose im)
  (test-diagonal im)
  (test-trace im)
  (test-identity im))

;; ======================================
;; Instance test function
;;
;; Implementations can call to test specific instances of interest
;;
;; All matrix implementations must pass this test for any valid matrix
(defn instance-test [m]
  (test-array-assumptions m))

;; ==============================================
;; General NDArray test
;;
;; These are the most general tests for general purpose mutable NDArray objects
;;
;; A general purpose NDArray implementation must pass this test to demonstrate
;; that is supports all core.matrix functionality correctly

(defn test-ndarray-implementation 
  "Tests a complete NDArray implementation"
  [im]
  (doseq [dim (range 10)] (is (supports-dimensionality? im dim)))
  (doseq [m (create-supported-matrices im)] (instance-test m))
  (instance-test (coerce im ['a 'b]))
  (instance-test (coerce im [[[[[["foo"]]]]]])))

;; ======================================
;; Main compliance test method
;; 
;; Implementations should call this with either a valid instance or their registered implementation key
;;
;; All valid core.matrix implementation must pass this test

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
