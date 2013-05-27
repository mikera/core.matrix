(ns clojure.core.matrix.test-api
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require clojure.core.matrix.examples)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (mget [1 2 3] 0)))
    (is (== 1 (mget [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (mget [[[1 2] [3 4]] [[5 6] [7 8]]] 1 1 1)))))

(deftest test-shape
  (is (= 0 (count (shape 1))))
  (is (= [2] (seq (int-array [2]))))
  (is (same-shape? [1 2] [3 4]))
  (is (same-shape? 0 1))
  (is (not (same-shape? [1 2] [2 3 4])))
  (is (not (same-shape? [1 2] [[0 1] [2 3]])))) 

(deftest test-as-vector
  (is (e== [1] (as-vector 1)))) 

(deftest test-implementations
  (testing "vector implementation"
    (is (clojure.core/vector? (imp/get-canonical-object :persistent-vector)))
    (is (= :persistent-vector (imp/get-implementation-key []))))
  (testing "non-existent implementation"
    (is (thrown? Throwable (imp/get-canonical-object :random-fictitious-implementation-key))))
  (testing "with-implementation"
    (is (= [1 2] (with-implementation [] (matrix [1 2]))))
    (is (= (class (double-array [1 2])) 
           (class (with-implementation :double-array (matrix [1 2])))))))

(deftest test-products
  (is (equals 1 (inner-product [0 1 1] [1 1 0])))
  (is (equals [[2 4] [6 8]] (inner-product [[2 0] [0 2]] [[1 2] [3 4]])))
  (is (equals [3 6] (outer-product 3 [1 2])))
  (is (equals [3 6] (outer-product [1 2] 3)))
  (is (equals [[1 2] [3 6]] (outer-product [1 3] [1 2]))))

(deftest test-add-products
  (is (equals 7 (add-product 1 2 3)))
  (is (equals [7] (add-product [1] 2 3)))
  (is (equals [3 8] (add-product [0 0] [1 2] [3 4])))) 

(deftest test-new
  (is (equals [0 0 0] (new-vector 3)))
  (is (= [0.0 0.0 0.0] (seq (new-vector :double-array 3))))
  (is (e= [0.0 0.0 0.0] (new-vector :double-array 3)))) 

(deftest test-compute-matrix
  (is (= [["00" "01"] ["10" "11"]]
         (compute-matrix :persistent-vector [2 2] str))))

;; TODO: need to fix and have proper errors!
(deftest test-shape-errors
  (is (error? (add [1] [2 3])))) 

(deftest test-coerce
  (testing "clojure vector coercion"
    (is (== 1.0 (coerce [] 1)))
    (is (= [1 2 3] (coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (coerce [1] [[1 2] [3 4]])))
    (is (= [[1 2] [3 4]] (coerce [1] '((1 2) (3 4)))))))
(deftest test-slices
  (testing "rows and columns of clojure vector matrix"
    (is (= [1 2 3] (get-row [[1 2 3] [4 5 6]] 0)))
    (is (= [2 5] (get-column [[1 2 3] [4 5 6]] 1))))
  (testing "get-nd on scalar with zero dimensions"
    (is (== 10.0 (mget 10.0)))
    (is (== 10.0 (mp/get-nd 10.0 []))))
  (testing "slices of clojure vector are scalar numbers"
    (is (= [1 2 3] (slices [1 2 3])))))

(deftest test-submatrix
  (is (equals [[3]] (submatrix [[1 2] [3 4]] [[1 1] [0 1]])))
  (is (equals [[2] [4]] (submatrix [[1 2] [3 4]] 1 [1 1])))
  (is (equals [2 3] (submatrix [1 2 3 4] [[1 2]])))
  (is (equals [2 3] (submatrix [1 2 3 4] 0 [1 2])))) 

(deftest test-element-seq
  (is (= [0] (eseq 0)))
  (is (= [2] (eseq [[2]])))
  (is (= [4] (eseq [[[[4]]]]))))

(deftest test-conforming?
  (is (conforming? 1 [[2 2] [3 3]]))
  (is (conforming? 1 [3 3]))
  (is (conforming? [3 3] 1))
  (is (conforming? [3 3] [[1 2] [3 4]]))
  (is (not (conforming? [3 3] [[1 2 3] [3 4 3]])))
  (is (not (conforming? [1 2] [3 4 5])))) 

(deftest test-broadcast
  (is (= [[1 1] [1 1]] (coerce [] (broadcast 1 [2 2]))))
  (is (equals [[[[2]]]] (broadcast 2 [1 1 1 1])))
  (is (= [2 2] (add [1 1] 1))))

(deftest test-mutable-matrix
  (is (error? (scale! [1 2] 2)))
  (is (equals (scale! (mutable-matrix [1 2]) 2) [2 4]))) 

(deftest test-reshape
  (is (equals 1 (reshape [1 2 3] [])))
  (is (equals [1 2 3 4] (reshape [[1.0 2.0] [3.0 4.0]] [4])))
  (is (equals [1 2] (reshape [[1.0 2.0] [3.0 4.0]] [2])))
  (is (equals [] (reshape [[1.0 2.0] [3.0 4.0]] [0])))
  (is (equals 1.0 (reshape [[1.0 2.0] [3.0 4.0]] [])))
  (is (equals [[1 2] [3 4]] (reshape [1 2 3 4] [2 2])))
  (testing "exceptions"
    (is (thrown? Throwable (reshape 1 [2])))
    (is (thrown? Throwable (reshape [1] [2 2]))))) 

(deftest test-index-seq
  (is (= [] (index-seq [])))
  (is (= [[]] (index-seq 10)))
  (is (= [[0] [1] [2]] (index-seq [1 2 3])))
  (is (= [[0 0] [0 1] [1 0] [1 1]] (index-seq [[1 2] [3 4]]))))

(deftest test-functional-ops
  (testing "eseq"
    (is (= [1] (eseq 1)))
    (is (= [1] (eseq [1])))
    (is (= [1] (eseq [[1]])))
    (is (= [1] (eseq [[[1]]])))))

(deftest test-equals
  (testing "scalars"
    (is (equals 6 6.0))
    (is (equals 6 (mul 3 2)))
    (is (equals 6.0 (scale 3 2))))
  (testing "vectors"
    (is (not (equals [6] [3])))
    (is (equals [6] [6.0]))
    (is (equals [6] (mul [3] 2)))
    (is (equals [6.0] (mul 2 [3]))))
  (testing "matrices"
    (is (equals [[1 0.0] [0 1.0]] [[1.0 0] [0.0 1]])))
  (testing "element e="
    (is (e= [1 2] [1 2]))
    (is (e= [1 2] [1 2] [1 2] [1 2]))
    (is (not (e= [1 2] [3 4])))
    (is (not (e= [1 2] [1.0 2.0])))
    (is (not (e= [1 2] [1 2] [1 3] [1 2]))))
  (testing "=="
    (is (op/== 2 2))
    (is (not (op/== 2 4)))
    (is (op/== [1 2] [1.0 2.0])))
  (testing "nil equality"
    (is (op/== nil nil))
    (is (not (op/== nil [nil])))
    (is (not (op/== nil []))))
  (testing "unequal lengths"
    (is (not (equals [1] [1 2])))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (mul 3 2)))
    (is (== 6 (scale 3 2)))
    (is (== 6 (mp/pre-scale 3 2))))
  (testing "matrix scaling"
    (is (= [6] (mul [3] 2)))
    (is (= [6] (mul 2 [3])))
    (is (= [[6]] (mul 2 [[3]])))
    (is (= [[6]] (mul [[2]] 3)))))

(deftest test-broadcast-compatibile
  (is (equals [[2 1] [2 2]] (mp/broadcast-compatible [2 1] 2))))

(deftest test-divide
  (is (== 2 (div 4 2)))
  (is (op/== [2 1] (div [4 2] 2)))
  (is (op/== [1 1.5] (div [2 3] 2)))
  (is (equals [2 1] (div 4 [2 4])))
  (is (equals [[1 2] [2 1]] (div [[4 8] [4 4]] [[4 4] [2 4]]))))

(deftest test-pow
  (is (== 8 (pow 2 3)))
  (is (equals [0.5 2] (pow [2 0.5] -1)))) 

(deftest test-addition
  (testing "matrix addition"
    (is (= [5.0] (add [3.0] [2.0])))
    (is (= [[6.0]] (add [[2.0]] [[4.0]])))
    (is (= [[[6.0]]] (add [[[2.0]]] [[[4.0]]])))))

(deftest test-subtraction
  (testing "matrix subtraction"
    (is (= [1.0] (sub [3.0] [2.0])))
    (is (= [[8.0]] (sub [[12.0]] [[4.0]])))
    (is (= [[[8.0]]] (sub [[[12.0]]] [[[4.0]]]))))
  (testing "mutable sub"
    (let [v (mutable-matrix [10 10])]
      (sub! v [1 2] [1 2])
      (is (equals [8 6] v)))))

(deftest test-transpose
  (testing "transpose different dimensionalities"
    (is (= 1 (transpose 1)))
    (is (= [1.0] (transpose [1.0])))
    (is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]])))
    (is (= [[1] [2] [3]] (transpose [[1 2 3]])))))

(deftest test-det
  (testing "determinant"
    ;; (is (== 3 (det 3))) ;; TODO fix for Number
    ;; (is (== -1 (det [[0 1] [1 0]]))) ;; TODO standard implementation
    )) 

(deftest test-join
  (is (= [1 2 3] (join [1 2] 3)))
  (is (= [[1 1] [2 2] [3 3]] (join [[1 1]] [[2 2] [3 3]])))) 

(deftest test-main-diagonal
  (is (e== [1 2] (main-diagonal [[1 0] [4 2] [5 7]]))))

(deftest test-normalise
  (testing "vector normalise"
    (is (= [1.0] (normalise [1.0])))
    (is (= [1.0] (normalise [2.0])))
    (is (= [-1.0 0.0] (normalise [-2.0 0.0])))))

(deftest test-mathsops
  (testing "ops on scalars"
    (is (== 1.0 (floor 1.2)))
    (is (thrown? Throwable (floor! 1.2))))
  (testing "ops"
    (is (= [1.0 2.0] (floor [1.2 2.7]))))
  (testing "mutable maths ops"
    (is (error? (signum! [1 2])))
    (is (equals [1 0 1 -1] (signum! (double-array [1 0 2 -10])))))) 

(deftest test-scalar
  (testing "special scalars"
    (is (scalar? nil))
    (is (not (scalar? [1])))
    (is (not (scalar? (clojure.core.matrix.impl.wrappers/wrap-scalar 1)))))
  (testing "numbers as scalars"
    (is (scalar? 1))
    (is (scalar? 1.0))
    (is (scalar? 1/7)))
  (testing "scalar dimensionality"
    (is (== 0 (dimensionality 1.0)))
    (is (== 1.0 (mget 1.0)))
    (is (= [] (shape 1.0))))
  (testing "functional operations"
    (is (= 2.0 (emap inc 1.0)))
    (is (= 10.0 (emap + 4.0 6.0)))
    (is (= 10.0 (emap + 1.0 2.0 3.0 4.0)))
    (is (== 10.0 (ereduce #(+ %1 %2) 10.0)))
    (is (== 3.0 (ereduce + 1.0 2.0)))
    (is (= [1.0] (eseq 1.0))))
  (testing "scalar operations"
    (is (== 10 (inner-product 2 5)))
    (is (== 10 (outer-product 2 5)))
    (is (== 10 (scale 2 5)))
    (is (== 10 (mp/pre-scale 2 5)))))

(deftest test-vector-ops
  (testing "vector dot product"
    (is (== 1.0 (dot [1.0] [1.0])))
    (is (== -1.0 (dot [1 2] [1 -1]))))
  (testing "vector distance"
    (is (== 1.0 (distance [0 0][0 1])))
    (is (== 1.0 (distance [1 0][0 0])))))

(deftest test-dimensions
  (testing "vector dimensions"
    (is (= 3 (row-count [1 2 3])))
    (is (= 3 (row-count [[1 2] [2 3] [3 4]])))
    (is (= 2 (column-count [[1 2] [2 3] [3 4]])))
    (is (= [3 2] (shape [[1 2] [2 3] [3 4]])))
    (is (= [2 2 2] (shape [[[1 2] [2 3]] [[3 4] [5 6]]])))))

(deftest test-broadcasting
  (testing "broadcast shapes"
    (is (nil? (broadcast-shape [1 2] [1 3])))
    (is (= [2 2] (broadcast-shape [1 2] [2 1])))
    (is (= [1 2 3] (broadcast-shape [1 2 3] [2 1])))
    (is (= [1 2 3 4] (broadcast-shape [1 2 3 1] [2 1 4])))
    (is (nil? (broadcast-shape [1 2 3 4] [2 3])))
    (is (= [] (broadcast-shape [] [])))
    (is (e= [[[nil]]] (broadcast nil [1 1 1]))))
  (testing "broadcasted ops"
    (is (e== [2 3] (add [1 2] 1.0)))
    (is (e== [2 3] (add 1.0 [1 2])))
    (is (e== [0 1] (sub [1 2] 1.0)))
    (is (e== [0 -1] (sub 1.0 [1 2]))))) 

(deftest test-object-array
  (is (e= [:a :b] (coerce [] (object-array [:a :b]))))) 

(deftest check-examples
  (binding [*out* (java.io.StringWriter.)]
    (testing "example code"
      (clojure.core.matrix.examples/all-examples))))

(deftest test-predicates
  (testing "scalar predicates"
    (is (not (array? 1)))
    (is (scalar? 1))
    (is (scalar? (mget [1 2 3] 1)))
    (is (scalar? (first (slices [1 2 3])))))
  (testing "clojure vector predicates"
    (is (array? [1 2]))
    (is (vec? [1 2]))
    (is (array? [[1 2] [3 4]]))
    (is (matrix? [[1 2] [3 4]]))
    (is (not (vec? [[1 2] [3 4]])))
    (is (not (matrix? [[[1 2] [2 3]] [[3 4] [5 6]]]))))
  (testing "row and column predicates"
    (is (not (column-matrix? [1])))
    (is (column-matrix? [[1]]))
    (is (not (row-matrix? [1])))
    (is (row-matrix? [[1]]))
    (is (not (column-matrix? [1 2])))
    (is (column-matrix? [[1] [2]]))
    (is (not (column-matrix? [[1 2 3]])))
    (is (row-matrix? [[1 2 3]]))
    (is (not (row-matrix? [1 2]))))
  (testing "mutability"
    (is (not (mutable? [1 2])))
    (is (mutable? (double-array [1 2])))))
