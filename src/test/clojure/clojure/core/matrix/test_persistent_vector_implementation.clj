(ns clojure.core.matrix.test-persistent-vector-implementation
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

;; Tests for the implementation of core.matrix on Clojure persistent vectors
;;
;; This is an important implementation because of the prevalence of vectors
;; in idiomatic Clojure code. It is also the primary immutable array implementation

(deftest test-regressions
  (testing "vector 3D transpose"
    (is (= [[[1]]] (transpose [[[1]]]))))
  (testing "vector wrapper coerce"
    (is (= 1.0 (coerce [] (wrap/wrap-scalar 1.0))))
    (is (= [1.0 2.0] (coerce [] (slices (double-array [1 2]))))))
  (testing "vector length"
    (is (== 5 (length [3 4]))))
  (testing "scalar broadcast"
    (is (e= [11 12 13] (add [1 2 3] 10)))
    (is (e= [11 12 13] (add 10 [1 2 3]))))
  (testing "persistent vector shape"
    (is (= [2] (seq (shape [1 2]))))
    (is (= [0] (seq (shape [])))))
  (testing "empty vector"
    (is (e= [] (coerce [] [])))
    (is (e= [] (assign [] 1.0)))
    (is (empty? (eseq [])))
    (is (nil? (coerce [] nil))))
  (testing "broadcast on emap"
    (is (equals [[6 7] [8 9]] (emap + [[1 2] [3 4]] 5)))
    (is (equals [[6 7] [8 9]] (emap + 5 [[1 2] [3 4]])))))

(deftest test-assign 
  (is (= [[1 2] [1 2]] (assign [[1 2] [3 4]] [1 2])))
  (is (error? (assign [1 2] [[1 2] [3 4]]))))

(deftest test-construction
  (is (equals [[0 0] [0 0]] (zero-array [] [2 2]))))

(deftest test-properties
  (is (numerical? [2 43]))
  (is (not (mutable? [1 2])))
  (is (not (mutable? [[1 2] [3 4]])))
  (testing "trace"
    (is (== 5 (trace [[1 2] [3 4]])))))

(deftest test-transpose
  (testing "vector transpose"
    (is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]])))
    (is (= [1 2 3] (transpose [1 2 3])))
    (is (= [[[[1]]]] (transpose [[[[1]]]])))))

(deftest test-broadcast
  (is (equals [[1 2] [1 2]] (broadcast [1 2] [2 2]))))

(deftest test-rows-columns
  (is (equals [[1 2] [3 4]] (rows [[1 2] [3 4]])))
  (is (equals [[1 3] [2 4]] (columns [[1 2] [3 4]]))))

(deftest test-submatrix
  (is (equals [2 3] (submatrix [1 2 3] 0 [1 2]))))

(deftest test-rotate
  (is (equals [2 3 1] (rotate [1 2 3] 0 1))))

(deftest test-order
  (is (equals [1 3 3] (order [1 2 3 4] [0 2 2])))
  (is (equals [[1 4] [2 5]] (order [[1 2 3] [4 5 6]] 1 [0 1])))
  (is (error? (order [1 2] 1 [0])))
  (is (error? (order [1 2] [2]))))

(deftest test-dot
  (is (equals [2 4 6] (dot 2 [1 2 3])))
  (is (equals [2 4 6] (dot [1 2 3] 2)))
  (is (equals 20 (dot [1 2 3] [2 3 4])))
  (is (equals [[1 2] [6 8]] (dot [[1 0] [0 2]] [[1 2] [3 4]])))) 

(deftest test-incompatible
  (is (error? (add [1 2] [3 4 5])))
  (is (error? (sub [[1] [2]] [[3] [4] [5]])))
  (is (error? (emul [[1] [2]] [[3] [4] [5]]))))

(deftest test-functional-op
  (testing "map"
    (is (= 2 (emap inc 1)))
    (is (= [1 2] (emap inc [0 1])))
    (is (= [1 2] (emap + [0 1] [1 1])))
    (is (= [3 5] (emap + [0 1] [1 1] [0 0] [2 3])))
    (is (= [[2.0 0.0] [0.0 2.0]] (emap #(* 2 %) [[1.0 0.0] [0.0 1.0]])))))

(deftest test-matrix-multiply
  (testing "matrix multiplication"
    (is (= [[5 10] [15 20]] (mmul [[1 2] [3 4]] 5)))
    (is (= [[1 0] [2 2] [5 0]] (mmul [[1 0] [0 2] [5 0]] [[1 0] [1 1]])))
    (is (= [[1 2] [3 4]] (mmul [[1 2] [3 4]] [[1 0] [0 1]])))
    (is (= [[5]] (mmul [[1 2]] [[1] [2]])))
    (is (= [7 10] (mmul [1 2] [[1 2] [3 4]]))))
  (testing "elementwise multiplication"
    (is (= [2 4] (mul [1 2] 2)))))

(deftest test-division
  (testing "unary division"
    (is (== 0.5 (div 2))))
  (testing "vector elementwise division"
     (is (= [2 4] (div [4 4] [2 1])))))

(deftest test-transform
  (testing "matrix transform"
    (is (= [5 10] (transform [[1 0] [0 2]] [5 5]))))
  (testing "function transform"
    (is (= [1 2] (transform (fn [_] [1 2]) [5 5])))))

(deftest test-nested-implementation
  (testing "nested double arrays"
    (let [m [(double-array [1 2]) (double-array [3 4])]]
      (is (not (mutable? m))) ;; persistent vector should not be mutable, even if components are
      (is (== 2 (dimensionality m)))
      (is (equals [3 7] (mmul m [1 1])))
      (is (equals [2 4] (get-column m 1))))))

(deftest test-emap
  (testing "basic"
    (equals [2 3] (emap inc [1 2])))
  (testing "nested implementations"
    (equals [[2 3]] (emap inc [(double-array [1 2])]))
    (equals [[2 3]] (emap inc [[(wrap/wrap-scalar 1) (wrap/wrap-scalar 2)]]))))

(deftest test-eseq
  (testing "basic"
    (= [2 3] (eseq [2 3])))
  (testing "nested implementations"
    (= [1 2] (eseq [[1 2]]))
    (= [1 2] (eseq [[1] [2]]))
    (= [1 2] (eseq [(double-array [1 2])]))
    (= [1 2] (eseq [[(wrap/wrap-scalar 1) (wrap/wrap-scalar 2)]]))))

(deftest test-contained-scalar-array
  (let [a [(scalar-array 1) 2]]
    (is (== 1 (dimensionality a)))
    (is (vec? a))
    (is (= [1 2] (slices a)))))

(deftest test-scalar-interop
  (is (equals [2 4] (mul [1 2] (scalar-array 2)))))

(deftest test-row-setting
  (is (equals [[1 2] [5 5]] (set-row [[1 2] [3 4]] 1 5)))
  (is (equals [[1 2] [5 6]] (set-row [[1 2] [3 4]] 1 [5 6]))))

(deftest test-column-setting
  (is (equals [[1 5] [3 5]] (set-column [[1 2] [3 4]] 1 5)))
  (is (equals [[1 5] [3 6]] (set-column [[1 2] [3 4]] 1 [5 6]))))

(deftest test-slices
  (is (= [1 2] (slices [1 2])))
  (is (== 1 (first (slices [1 2]))))
  (is (error? (slice [1 2 3] 1 1))))

(deftest test-sum
  (testing "summing"
    (is (= 2.0 (esum [[1.0 0.0] [0.0 1.0]])))
    (is (= 1.5 (esum [1.0 0.5])))))

(deftest test-coerce
  (testing "self-coerce"
    (is (= [2] (coerce [] [2]))))
  (testing "double arrays"
    (is (= [1.0 2.0] (coerce [] (double-array [1 2])))))
  (testing "nested sequences"
    (is (= [[1 2] [3 4]] (coerce [] '((1 2) (3 4)))))))

(deftest test-row-operations
    (testing "vector row swap"
      (is (= (matrix [0 2]) (swap-rows (matrix [2 0]) 0 1)))
      (is (= (matrix [0 2]) (swap-rows (matrix [2 0]) 1 0))))
    (testing "matrix row swap"
      (is (= (matrix [[0 2] [2 0]]) (swap-rows (matrix [[2 0] [0 2]]) 0 1)))
      (is (= (matrix [[0 2] [2 0] [1 1]]) (swap-rows (matrix [[1 1] [2 0] [0 2]]) 0 2))))
    (testing "multiply row i by constant k"
      (is (= (matrix [[0 2 4]]) (multiply-row (matrix [[0 1 2]]) 0 2))))
    (testing "add row j to i and replace i with the result"
      (is (= (matrix [[3 3] [1 1]]) (add-row (matrix [[1 1] [1 1]]) 0 1 2)))))

(deftest test-bad-shapes
  (is (error? (array [1 [2 3]])))
  (is (error? (array [[1 2] [2 3 4]])))
  (is (error? (array [[1 2 3 4] [2 3 4]])))
  (is (error? (array [[1 2 3 4] 5]))))

;; run complicance tests

(deftest instance-tests
  (testing "empty persistent vectors are supported"
    (clojure.core.matrix.compliance-tester/instance-test []))
  (testing "matrices of symbols are supported"
    (clojure.core.matrix.compliance-tester/instance-test ['a 'b]))
  (testing "matrices of heterogeneous submatrices"
    (clojure.core.matrix.compliance-tester/instance-test [[1 2.0] (double-array [3 4])])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test [1]))
