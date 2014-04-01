(ns clojure.core.matrix.test-api
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:use clojure.core.matrix.select)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require clojure.core.matrix.examples)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?])
  (:use clojure.test))

;; This namespace is intended for general purpose tests og the core.matrix API functions

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (mget [1 2 3] 0)))
    (is (== 1 (mget [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (mget [[[1 2] [3 4]] [[5 6] [7 8]]] 1 1 1)))))

(deftest test-ml-style-indexing
  (let [a [[1 2] [3 4]]]
    (testing "higher level indexing"
      (is (== 1 (sel a 0 0)))
      (is (= [[1] [3]] (sel a [0 1] 0)))
      (is (= a (sel a (irange) (irange))))
      (is (== 4 (sel a end end)))
      (is (== 2 (sel a (exclude 1) (exclude 0))))
      (is (= [1 2] (sel [[-1 0] [1 2]] (where pos?)))))))

(deftest test-shape
  (testing "basic array shapes"
           (is (= 0 (count (shape 1))))
           (is (= [2] (seq (int-array [2])))))
  (testing "same shape function"
    (is (same-shape? [1 2] [3 4]))
    (is (same-shape? 0 1))
    (is (same-shape? "Foo" nil))
    (is (not (same-shape? [1 2] [2 3 4])))
    (is (not (same-shape? [1 2] [[0 1] [2 3]])))))

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
  (is (equals 110 (inner-product [0 2 3] 5 [7 11 0])))
  (is (equals [[2 4] [6 8]] (inner-product [[2 0] [0 2]] [[1 2] [3 4]])))
  (is (equals [3 6] (outer-product 3 [1 2])))
  (is (equals [3 6] (outer-product [1 2] 3)))
  (is (equals [[1 2] [3 6]] (outer-product [1 3] [1 2]))))

(deftest test-add-products
  (is (equals 7 (add-product 1 2 3)))
  (is (equals [7] (add-product [1] 2 3)))
  (is (equals [3 8] (add-product [0 0] [1 2] [3 4]))))

(deftest test-reshape
  (is (equals [[0 1] [2 3] [4 5]] (reshape (range 6) [3 2])))) 

(deftest test-square
  (is (equals 81 (square 9)))
  (is (equals [1 4] (square [1 2])))
  (is (equals [[1 4]] (square [(double-array [1 2])]))))

(deftest test-new
  (is (e= [0.0] (new-vector :ndarray 1)))
  (is (e= [[0.0]] (new-matrix :ndarray 1 1)))
  (is (e= [nil] (new-array :ndarray [1])))
  (is (equals [0 0 0] (new-vector :persistent-vector 3)))
  (is (equals [0 0 0] (new-vector :ndarray-double 3)))
  (is (= [0.0 0.0 0.0] (seq (new-vector :double-array 3))))
  (is (e= [0.0 0.0 0.0] (new-vector :double-array 3))))

(deftest test-compute-matrix
  (is (= [["00" "01"] ["10" "11"]]
         (compute-matrix :persistent-vector [2 2] str))))

(deftest test-shape-errors
  (is (error? (add [1] [2 3]))))

(deftest test-mutable-matrix-fill
  (let [m [1 2 3]
        mm (mutable-matrix m)]
    (fill! mm 0.5)
    (is (equals mm [0.5 0.5 0.5]))))

(deftest test-mutable-matrix-assign
  (let [m [1 2 3]
        mm (mutable-matrix m)]
    (assign! mm 0.5)
    (is (equals mm [0.5 0.5 0.5]))))

(deftest test-coerce
  (testing "clojure vector coercion"
    (is (== 1.0 (coerce [] 1)))
    (is (= [1 2 3] (coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (coerce [1] [[1 2] [3 4]])))
    (is (= [[1 2] [3 4]] (coerce [1] '((1 2) (3 4))))))
  (testing "coerce to a number"
     ;; (is (= 1 (coerce 2 1))) ;; TODO: what should happen here?
    )
  )

(deftest test-pow
  (let [a (array :persistent-vector [1 2 3])
        m (matrix :persistent-vector [[1 2 3] [4 5 6] [7 8 9]])]
    (testing "pow works on scalars"
      (is (== 8 (pow 2 3)))
      (is (== 8 (clojure.core.matrix.operators/** 2 3))))
    (testing "pow works when base is an array and exponent is a scalar"
      (is (equals [1.0 4.0 9.0] (pow a 2)))
      (is (equals [[1.0 4.0 9.0] [16.0 25.0 36.0] [49.0 64.0 81.0]] (pow m 2))))
    (testing "pow works when base is a scalar and exponent is an array"
      (is (equals [5.0 25.0 125.0] (pow 5 a))
          (equals [[2.0 4.0 8.0] [16.0 32.0 64.0] [128.0 256.0 512.0]] (pow 2 m))))
    (testing "pow works when both the base and the exponent are arrays"
      (is (equals [1.0 4.0 27.0] (pow a a)))
      (is (equals [[1.0 2.0 3.0] [16.0 25.0 36.0] [343.0 512.0 729.0]] (pow m a))))))

(deftest test-slices
  (testing "rows and columns of clojure vector matrix"
    (is (= [1 2 3] (get-row [[1 2 3] [4 5 6]] 0)))
    (is (= [2 5] (get-column [[1 2 3] [4 5 6]] 1))))
  (testing "get-nd on scalar with zero dimensions"
    (is (== 10.0 (mget 10.0)))
    (is (== 10.0 (mp/get-nd 10.0 []))))
  (testing "slices of a standard vector are scalar numbers"
    (is (= [1 2 3] (slices (array [1 2 3]))))))

(deftest test-submatrix
  (is (equals [[3]] (submatrix (array [[1 2] [3 4]]) [[1 1] [0 1]])))
  (is (equals [[2] [4]] (submatrix (array [[1 2] [3 4]]) 1 [1 1])))
  (is (equals [2 3] (submatrix (array [1 2 3 4]) [[1 2]])))
  (is (equals [[4]] (submatrix [[1 2] [3 4]] 1 1 1 1))) 
  (is (equals [2 3] (submatrix (array [1 2 3 4]) 0 [1 2]))))

(deftest test-element-seq
  (is (= [0] (eseq 0)))
  (is (= [1] (eseq [1])))
  (is (= [2] (eseq [[2]])))
  (is (= [4] (eseq [[[[4]]]]))))

(deftest test-element-map
  (is (equals 1 (emap inc (array 0))))
  (is (equals [2] (emap inc (array [1]))))
  (is (equals [[3]] (emap inc (array [[2]]))))
  (is (equals [[[[5]]]] (emap inc (array [[[[4]]]]))))
  (is (equals [10] (emap + [1] [2] [3] [4])))
  (is (equals [10] (emap + [1] (broadcast 2 [1]) (double-array [3]) [4]))))

(deftest test-conforming?
  (is (conforming? [[2 2] [3 3]] 1))
  (is (conforming? [[2 2] [3 3]] [1 1]))
  (is (conforming? [3 3] 1))
  (is (not (conforming? [3 3] [[1 2 3] [3 4 3]])))
  (is (not (conforming? [1 2] [3 4 5])))
  (is (not (conforming? [[0.0]] [0.0 0.0]))))

(deftest test-broadcast
  (is (= [[1 1] [1 1]] (coerce [] (broadcast 1 [2 2]))))
  (is (equals [[[[2]]]] (broadcast (array 2) [1 1 1 1])))
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
    (is (e= 'a 'a))
    (is (e= :foo :foo))
    (is (e= [1 2] (array [1 2])))
    (is (e= [1 2] [1 2] [1 2] [1 2]))
    (is (not (e= [1 2] [3 4])))
    (is (not (e= [1 2] [1.0 2.0])))
    (is (not (e= [1 2] [1 2] [1 3] [1 2]))))
  (testing "=="
    (is (op/== 2 2))
    (is (not (op/== 2 4)))
    (is (op/== [1 2] [1.0 2.0])))
  (testing "nil equality"
    (is (e= nil nil))
    (is (not (e= nil [nil])))
    (is (not (e= nil []))))
  (testing "unequal lengths"
    (is (not (equals [1] [1 2])))
    (is (not (e= [1] [1 2]))))
  (testing "equals does not broadcast"
    (is (not (equals (array 1) (array [1 1]))))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (mul 3 2)))
    (is (== 6 (scale 3 2)))
    (is (== 6 (mp/pre-scale 3 2))))
  (testing "matrix scaling"
    (is (equals [6] (mul (array [3]) 2)))
    (is (equals [6] (mul 2 (array [3]))))
    (is (equals [[6]] (mul 2 (array [[3]]))))
    (is (equals [[6]] (mul (array [[2]]) 3)))
    (is (equals [[6]] (mul (array 2) (array [[3]]))))
    (is (equals [[6]] (mul (array [[2]]) (array 3))))))

(deftest test-broadcast-compatibile
  (is (equals [[2 1] [2 2]] (mp/broadcast-compatible (array [2 1]) (array 2)))))

(deftest test-broadcast-like
  (is (equals [2 2] (mp/broadcast-like [1 1] 2)))
  (is (equals [2 2] (mp/broadcast-like [1 1] [2 2])))
  (is (equals [[7 7] [7 7]] (mp/broadcast-like [[1 2] [3 4]] 7)))
  (is (equals [1 2 3] (mp/broadcast-like [2 3 4] [1 2 3])))
  (is (error? (mp/broadcast-like [1 2 3] [1 2]))))

(deftest test-broadcast-coerce
  (is (equals [2 2] (mp/broadcast-coerce [1 1] 2)))
  (is (equals [2 2] (mp/broadcast-coerce [1 1] (double-array [2 2]))))
  (is (equals [[7 7] [7 7]] (mp/broadcast-coerce [[1 2] [3 4]] 7)))
  (is (equals [1 2 3] (mp/broadcast-coerce [2 3 4] [1 2 3])))
  (is (error? (mp/broadcast-coerce [1 2 3] [1 2]))))

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
  (testing "unary subtraction"
    (is (== (- 10) (op/- 10)))
    (is (equals (sub [1 2]) (op/- [1 2]))))
  (testing "matrix subtraction"
    (is (= [1.0] (sub (array [3.0]) [2.0])))
    (is (= [[8.0]] (sub (array [[12.0]]) [[4.0]])))
    (is (= [[[8.0]]] (sub (array [[[12.0]]]) [[[4.0]]]))))
  (testing "mutable sub"
    (let [v (mutable-matrix [10 10])]
      (sub! v [1 2] [1 2])
      (is (equals [8 6] v))))
  (testing "arity 3 sub regression"
    (is (equals [-1 -2] (sub [1 2] [1 2] [1 2])))))

(deftest test-transpose
  (testing "transpose different dimensionalities"
    (is (= 1 (transpose 1)))
    (is (= [1.0] (transpose [1.0])))
    (is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]])))
    (is (= [[1] [2] [3]] (transpose [[1 2 3]]))))
  (testing "in place transpose"
    (let [m [[1 2] [3 4]]]
      (is (e= (transpose m) (transpose! (mutable m)))))))

(deftest test-det
  (testing "determinant"
    ;; (is (== 3 (det 3))) ;; TODO fix for Number
    ;; (is (== -1 (det [[0 1] [1 0]]))) ;; TODO standard implementation
    ))

(deftest test-join
  (is (= [1 2 3] (join [1 2] 3)))
  (is (= [[1 1] [2 2] [3 3]] (join [[1 1]] [[2 2] [3 3]]))))

(deftest test-main-diagonal
  (is (e== [1 2] (main-diagonal [[1 0] [4 2] [5 7]])))
  (is (e== [1 4] (diagonal [[1 2] [3 4]]))))

(deftest test-diagonals
  ;; TODO: enable once diagonal function is complete
  ;; (is (e== [1 4] (diagonal [[1 2] [3 4]] 0)))
  ;; (is (e== [2] (diagonal [[1 2] [3 4]] 1)))
  ;; (is (e== [3] (diagonal [[1 2] [3 4]] -1)))
  ) 

(deftest test-diagonal
  (is (= [1 4] (diagonal [[1 2] [3 4] [5 6]]   )))
  (is (= [] (diagonal [[1 2] [3 4] [5 6]]  8)))
  (is (= [1 4] (diagonal [[1 2] [3 4] [5 6]]  0)))
  (is (= [2]   (diagonal [[1 2] [3 4] [5 6]]  1)))
  (is (= [3 6] (diagonal [[1 2] [3 4] [5 6]] -1)))
  (is (= [5]   (diagonal [[1 2] [3 4] [5 6]] -2))))

(deftest test-normalise
  (testing "vector normalise"
    (is (e== [1.0] (normalise (array [1.0]))))
    (is (e== [1.0] (normalise (array [2.0]))))
    (is (e== [-1.0 0.0] (normalise (array [-2.0 0.0]))))))

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
    (is (== 0 (dimensionality :foo)))
    (is (== 0 (dimensionality 'bar)))
    (is (== 1.0 (mget 1.0)))
    (is (nil? (shape 1.0))))
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
    (is (= [2 2 2] (shape [[[1 2] [2 3]] [[3 4] [5 6]]]))))
  (testing "element counts"
    (is (== 1 (ecount :foo)))))

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

(deftest test-sparsity 
  (testing "sparse?"
    (is (not (sparse? [0 1 2]))))
  (testing "density"
    (is (== 0.75 (density [0 1 2 3])))))

(deftest test-object-array
  (is (e= [:a :b] (coerce [] (object-array [:a :b]))))
  (let [a (to-object-array [1 2 3])]
    (is (= 2 (aget a 1)))))

(deftest test-permutation
  (is (equals [[0 1] [1 0]] (permutation-matrix [1 0])))
  (is (equals [[1 0] [0 1]] (permutation-matrix [0 1])))
  (is (equals [[0 1 0] [0 0 1] [1 0 0]] (permutation-matrix [1 2 0]))))

(deftest test-block-diagonal
  (is (= [[1]] (block-diagonal-matrix [[[1]]])))
  (is (= [[1 0.0] [0.0 2]] (block-diagonal-matrix [[[1]][[2]]])))
  (is (= [[1 0.0 0.0] [0.0 2 3] [0.0 4 5]] (block-diagonal-matrix [[[1]][[2 3][4 5]]]))))

(deftest check-examples
  (binding [*out* (java.io.StringWriter.)]
    (testing "example code"
      (clojure.core.matrix.examples/all-examples))))

(deftest test-zeros
  (is (zero-matrix? (zero-matrix 3 3)))
  (is (zero-matrix? (zero-vector 3)))
  (is (zero-matrix? (zero-array [2 2 2]))))

(deftest test-numerical
  (testing "numerical predicate"
    (is true)
    (is (clojure.core.matrix/numerical? 3))
    (is (numerical? [1 1.0 2N (float 0)]))
    (is (not (numerical? [1 :foo nil (float 0)])))
    (is (not (numerical? nil)))))

(deftest test-scalar-array
  (let [a (scalar-array 3)]
    (is (array? a))
    (is (equals 3 a))
    (is (equals 4 (mset a 4)))
    (is (equals 6 (add a a)))
    (is (= 3 (mget a)))
    (is (= 3 (scalar a))))
  (is (equals 0 (new-scalar-array))))

(deftest test-min-max
  (is (== 1 (emin [2 1 7])))
  (is (== 7 (emax [2 1 7])))
  (is (== 7 (emax [[4 3 2] [2 1 7] [-1 5 -20]]))))

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
    (is (mutable? (double-array [1 2]))))
  (testing "symmetry"
    (is (symmetric? (matrix [[1 -3][-3 2]])))
    (is (not (symmetric? (matrix [[1 -3][-10 2]]))))
    (is (symmetric? (matrix [[1 -4 -5][-4 2 -6][-5 -6 3]])))
    (is (not (symmetric? (matrix [[1 -4 -5][-4 2 -6][-5 -10 3]]))))
    (is (not (symmetric? (matrix [[1 2 3 4]]))))
    (is (not (symmetric? (matrix [[1][2][3][4]]))))
    (is (symmetric? (matrix [1 2 3 4])))
    (is (symmetric? 2))
    (is (symmetric? nil))
    (is (symmetric? (double-array [1 2 3 4])))
    (is (symmetric? (array [1 2 3 4])))
    (is (symmetric? (array [[1 -3][-3 2]])))
    (is (not (symmetric? (array [[1 -3][-10 2]]))))))

(deftest test-inplace-operators
  (is (op/== (matrix [5 7])
             (op/+= (mutable (matrix [1 2]))
                    (matrix [4 5]))))
  (is (op/== (matrix [-4 6]))
             (op/-= (mutable (matrix [5 8]))
                    (matrix [9 2])))
  (is (op/== (matrix [6 8])
             (op/*= (mutable (matrix [3 2]))
                    (matrix [2 4]))))
  (is (op/== (matrix [2 0.5])
             (op/div= (mutable (matrix [4 2]))
                    (matrix [2 4])))))
