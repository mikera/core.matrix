(ns clojure.core.matrix.test-api
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.linear :as li]
            [clojure.core.matrix.operators :as op]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.utils :refer [broadcast-shape]]
   #?@(:clj [[clojure.core.matrix.macros :refer [error]]
             [clojure.core.matrix.macros-clj :refer [error?]]
             [clojure.test :refer [deftest is testing run-tests]]
             [clojure.core.matrix.demo.examples]]
      :cljs [[cljs.test :refer-macros [deftest is testing run-tests]]
             [clojure.core.matrix :refer-macros [with-implementation]]
             [clojure.core.matrix.macros :refer-macros [error]]
             [clojure.core.matrix.macros-cljs :refer-macros [error?]]]))
  #?(:clj (:import [java.io StringWriter])))


;; This namespace is intended for general purpose tests of the core.matrix API functions

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (m/mget [1 2 3] 0)))
    (is (== 1 (m/mget [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (m/mget [[[1 2] [3 4]] [[5 6] [7 8]]] 1 1 1)))))

(deftest test-labels
  (testing "unlabelled array"
    (let [m [[7 8] [9 10]]]
      (is (nil? (m/label m 0 0)))
      (is (nil? (m/label m 0 1)))
      (is (nil? (m/labels m 0)))
      (is (nil? (m/labels m 1)))
      (is (error? (m/label m 0 2)))
      (is (error? (m/labels m -1))))))

(deftest test-select
  (let [a [[1 2] [3 4]]]
    (testing "higher level indexing"
      (is (m/equals 1 (m/select a 0 0)))
      (is (m/equals [[1] [3]] (m/select a [0 1] [0])))
      (is (m/equals [1 3] (m/select a :all 0)))
      (is (m/equals a (m/select a :all :all)))
      (is (m/equals [3] (m/select a [1] 0))))))

(deftest test-select-indices
  (let [a [[1 2] [3 4]]]
    (testing "select indices"
      (is (m/equals [1 4] (m/select-indices a [[0 0] [1 1]])))
      (is (m/equals [[5 2] [3 6]] (m/set-indices a [[0 0] [1 1]] [5 6])))
      (is (m/equals [[0 0] [0 0]] (m/set-indices a [[0 0] [0 1] [1 0] [1 1]] [0 0 0 0])))
      (is (m/equals [[0 0] [0 0]] (m/set-indices a [[0 0] [0 1] [1 0] [1 1]] 0)))
      (let [ma (m/mutable a)]
        (m/set-indices! ma [[0 0] [1 1]] [5 6])
        (is (m/equals ma [[5 2] [3 6]]))
        (is (m/equals (m/transpose ma) [[5 3] [2 6]]))))))

(deftest test-set-selection
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12]]]
    (testing "m/set-selection"
      (is (m/equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] (m/set-selection a 0 0 2)))
      (is (m/equals [[3 2 3 3] [5 6 7 8] [3 10 11 3]] (m/set-selection a [0 2] [0 3] 3))))))

(deftest test-set-selection!
  (let [a (m/matrix :ndarray [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
    (testing "sel-set!"
      (m/set-selection! a 0 0 2)
      (is (m/equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] a))
      (m/set-selection! a :all 0 0)
      (is (m/equals [[0 2 3 4] [0 6 7 8] [0 10 11 12]] a)))))

(deftest test-shape
  (testing "basic array shapes"
           (is (= 0 (count (m/shape 1))))
           (is (= [2] (seq (int-array [2])))))
  (testing "same shape function"
    (is (m/same-shape? [1 2] [3 4]))
    (is (m/same-shape? 0 1))
    (is (m/same-shape? "Foo" nil))
    (is (not (m/same-shape? [1 2] [2 3 4])))
    (is (not (m/same-shape? [1 2] [[0 1] [2 3]])))))

(deftest test-as-vector
  (is (m/e== [1] (m/as-vector 1))))

(deftest test-implementations
  (testing "vector implementation"
    (is (clojure.core/vector? (imp/get-canonical-object :persistent-vector)))
    (is (= :persistent-vector (imp/get-implementation-key []))))
  (testing "non-existent implementation"
    (is (nil? (imp/get-canonical-object :random-fictitious-implementation-key))))
  (testing "with-implementation"
    (is (= [1 2] (m/with-implementation [] (m/matrix [1 2]))))
    #?(:clj
    (is (= (class (double-array [1 2]))
           (class (m/with-implementation :double-array (m/matrix [1 2]))))))))

(deftest test-products
  (is (m/equals 1 (m/inner-product [0 1 1] [1 1 0])))
  (is (m/equals 110 (m/inner-product [0 2 3] 5 [7 11 0])))
  (is (m/equals [[2 4] [6 8]] (m/inner-product [[2 0] [0 2]] [[1 2] [3 4]])))
  (is (m/equals [3 6] (m/outer-product 3 [1 2])))
  (is (m/equals [3 6] (m/outer-product [1 2] 3)))
  (is (m/equals [[1 2] [3 6]] (m/outer-product [1 3] [1 2]))))

(deftest test-add-products
  (is (m/equals 7 (m/add-product 1 2 3)))
  (is (m/equals [7] (m/add-product [1] 2 3)))
  (is (m/equals [3 8] (m/add-product [0 0] [1 2] [3 4]))))

(deftest test-reshape
  (is (m/equals [[0 1] [2 3] [4 5]] (m/reshape (range 6) [3 2]))))

(deftest test-square
  (is (m/equals 81 (m/square 9)))
  (is (m/equals [1 4] (m/square [1 2])))
  (is (m/equals [[1 4]] (m/square [(double-array [1 2])]))))

(deftest test-new
  (is (m/e= [0.0] (m/new-vector :ndarray 1)))
  (is (m/e= [[0.0]] (m/new-matrix :ndarray 1 1)))
  (is (m/e= [nil] (m/new-array :ndarray [1])))
  (is (m/equals [0 0 0] (m/new-vector :persistent-vector 3)))
  (is (m/equals [0 0 0] (m/new-vector :ndarray-double 3)))
  (is (= [0.0 0.0 0.0] (seq (m/new-vector :double-array 3))))
  (is (m/e= [0.0 0.0 0.0] (m/new-vector :double-array 3))))

(deftest test-compute-matrix
  (is (= [["00" "01"] ["10" "11"]]
         (m/compute-matrix :persistent-vector [2 2] str))))

(deftest test-shape-errors
  (is (error? (m/add [1] [2 3]))))

(deftest test-mutable-matrix-fill
  (let [m [1 2 3]
        mm (m/mutable m)]
    (m/fill! mm 0.5)
    (is (m/equals mm [0.5 0.5 0.5]))))

(deftest test-mutable-matrix-assign
  (let [m [1 2 3]
        mm (m/mutable m)]
    (m/assign! mm 0.5)
    (is (m/equals mm [0.5 0.5 0.5]))))

(deftest test-coerce
  (testing "clojure vector coercion"
    (is (== 1.0 (m/coerce [] 1)))
    (is (= [1 2 3] (m/coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (m/coerce [1] [[1 2] [3 4]])))
    (is (= [[1 2] [3 4]] (m/coerce [1] '((1 2) (3 4))))))
  (testing "coerce to a number"
     (is (= 1 (m/coerce 2 1)))
     (is (m/equals [1 2] (m/coerce 2 [1 2])))))

(deftest test-pow
  (let [a (m/array :persistent-vector [1 2 3])
        m (m/matrix :persistent-vector [[1 2 3] [4 5 6] [7 8 9]])]
    (testing "pow works on scalars"
      (is (== 8 (m/pow 2 3)))
      (is (== 8 (clojure.core.matrix.operators/** 2 3))))
    (testing "pow works when base is an array and exponent is a scalar"
      (is (m/equals [1.0 4.0 9.0] (m/pow a 2)))
      (is (m/equals [[1.0 4.0 9.0] [16.0 25.0 36.0] [49.0 64.0 81.0]] (m/pow m 2))))
    (testing "pow works when base is a scalar and exponent is an array"
      (is (m/equals [5.0 25.0 125.0] (m/pow 5 a)))
      (is (m/equals [[2.0 4.0 8.0] [16.0 32.0 64.0] [128.0 256.0 512.0]] (m/pow 2 m))))
    (testing "pow works when both the base and the exponent are arrays"
      (is (m/equals [1.0 4.0 27.0] (m/pow a a)))
      (is (m/equals [[1.0 4.0 27.0] [4.0 25.0 216.0] [7.0 64.0 729.0]] (m/pow m a))))))

(deftest test-slices
  (testing "rows and columns of clojure vector matrix"
    (is (= [1 2 3] (m/get-row [[1 2 3] [4 5 6]] 0)))
    (is (= [2 5] (m/get-column [[1 2 3] [4 5 6]] 1))))
  (testing "get-nd on scalar with zero dimensions"
    (is (== 10.0 (m/mget 10.0)))
    (is (== 10.0 (mp/get-nd 10.0 []))))
  (testing "slices of a standard vector are scalar numbers"
    (is (= [1 2 3] (m/slices (m/array [1 2 3]))))))

(deftest test-slice-on-1d
  (testing "slice on 1d must return scalar"
    (is (m/scalar? (m/slice [1 2 3] 0)))))

(deftest test-submatrix
  (is (m/equals [[3]] (m/submatrix (m/array [[1 2] [3 4]]) [[1 1] [0 1]])))
  (is (m/equals [[2] [4]] (m/submatrix (m/array [[1 2] [3 4]]) 1 [1 1])))
  (is (m/equals [2 3] (m/submatrix (m/array [1 2 3 4]) [[1 2]])))
  (is (m/equals [[4]] (m/submatrix [[1 2] [3 4]] 1 1 1 1)))
  (is (m/equals [2 3] (m/submatrix (m/array [1 2 3 4]) 0 [1 2]))))

(deftest test-element-seq
  (is (= [0] (m/eseq 0)))
  (is (= [1] (m/eseq [1])))
  (is (= [2] (m/eseq [[2]])))
  (is (= [4] (m/eseq [[[[4]]]]))))

(deftest test-element-map
  (is (m/equals 1 (m/emap inc (m/array 0))))
  (is (m/equals [2] (m/emap inc (m/array [1]))))
  (is (m/equals [[3]] (m/emap inc (m/array [[2]]))))
  (is (m/equals [[[[5]]]] (m/emap inc (m/array [[[[4]]]]))))
  (is (m/equals [10] (m/emap + [1] [2] [3] [4])))
  (is (m/equals [10] (m/emap + [1] (m/broadcast 2 [1]) (double-array [3]) [4]))))

(deftest test-conforming?
  (is (m/conforming? [[2 2] [3 3]] 1))
  (is (m/conforming? [[2 2] [3 3]] [1 1]))
  (is (m/conforming? [3 3] 1))
  (is (not (m/conforming? [3 3] [[1 2 3] [3 4 3]])))
  (is (not (m/conforming? [1 2] [3 4 5])))
  (is (not (m/conforming? [[0.0]] [0.0 0.0]))))

(deftest test-broadcast
  (is (= [[1 1] [1 1]] (m/coerce [] (m/broadcast 1 [2 2]))))
  (is (m/equals [[[[2]]]] (m/broadcast (m/array 2) [1 1 1 1])))
  (is (= [2 2] (m/add [1 1] 1))))

(deftest test-mutable-matrix
  (is (error? (m/scale! [1 2] 2)))
  (is (m/equals (m/scale! (m/mutable [1 2]) 2) [2 4])))

(deftest test-reshape-2
  (is (m/equals 1 (m/reshape [1 2 3] [])))
  (is (m/equals [1 2 3 4] (m/reshape [[1.0 2.0] [3.0 4.0]] [4])))
  (is (m/equals [1 2] (m/reshape [[1.0 2.0] [3.0 4.0]] [2])))
  (is (m/equals [] (m/reshape [[1.0 2.0] [3.0 4.0]] [0])))
  (is (m/equals 1.0 (m/reshape [[1.0 2.0] [3.0 4.0]] [])))
  (is (m/equals [[1 2] [3 4]] (m/reshape [1 2 3 4] [2 2])))
  (is (m/equals [1 0] (m/reshape 1 [2])))
  (is (m/equals [[1 2] [3 0]] (m/reshape [1 2 3] [2 2]))))

(deftest test-index-seq
  (is (= [] (m/index-seq [])))
  (is (= [[]] (m/index-seq 10)))
  (is (= [[0] [1] [2]] (m/index-seq [1 2 3])))
  (is (= [[0 0] [0 1] [1 0] [1 1]] (m/index-seq [[1 2] [3 4]]))))

(deftest test-functional-ops
  (testing "m/eseq"
    (is (= [1] (m/eseq 1)))
    (is (= [1] (m/eseq [1])))
    (is (= [1] (m/eseq [[1]])))
    (is (= [1] (m/eseq [[[1]]])))))

(deftest test-equals
  (testing "scalars"
    (is (m/equals 6 6.0))
    (is (m/equals 6 (m/mul 3 2)))
    (is (m/equals 6.0 (m/scale 3 2))))
  (testing "vectors"
    (is (not (m/equals [6] [3])))
    (is (m/equals [6] [6.0]))
    (is (m/equals [6] (m/mul [3] 2)))
    (is (m/equals [6.0] (m/mul 2 [3]))))
  (testing "matrices"
    (is (m/equals [[1 0.0] [0 1.0]] [[1.0 0] [0.0 1]])))
  (testing "element e="
    (is (m/e= 'a 'a))
    (is (m/e= :foo :foo))
    (is (m/e= [1 2] (m/array [1 2])))
    (is (m/e= [1 2] [1 2] [1 2] [1 2]))
    (is (not (m/e= [1 2] [3 4])))
    (is (not (m/e= [1 2] [1.0 2.0])))
    (is (not (m/e= [1 2] [1 2] [1 3] [1 2]))))
  (testing "=="
    (is (op/== 2 2))
    (is (not (op/== 2 4)))
    (is (op/== [1 2] [1.0 2.0])))
  (testing "nil equality"
    (is (m/e= nil nil))
    (is (not (m/e= nil [nil])))
    (is (not (m/e= nil []))))
  (testing "unequal lengths"
    (is (not (m/equals [1] [1 2])))
    (is (not (m/e= [1] [1 2]))))
  (testing "m/equals does not broadcast"
    (is (not (m/equals (m/array 1) (m/array [1 1]))))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (m/mul 3 2)))
    (is (== 6 (m/scale 3 2)))
    (is (== 6 (mp/pre-scale 3 2))))
  (testing "matrix scaling"
    (is (m/equals [6] (m/mul (m/array [3]) 2)))
    (is (m/equals [6] (m/mul 2 (m/array [3]))))
    (is (m/equals [[6]] (m/mul 2 (m/array [[3]]))))
    (is (m/equals [[6]] (m/mul (m/array [[2]]) 3)))
    (is (m/equals [[6]] (m/mul (m/array 2) (m/array [[3]]))))
    (is (m/equals [[6]] (m/mul (m/array [[2]]) (m/array 3))))))

(deftest test-broadcast-compatibile
  (is (m/equals [[2 1] [2 2]] (mp/broadcast-compatible (m/array [2 1]) (m/array 2)))))

(deftest test-broadcast-like
  (is (m/equals [2 2] (mp/broadcast-like [1 1] 2)))
  (is (m/equals [2 2] (mp/broadcast-like [1 1] [2 2])))
  (is (m/equals [[7 7] [7 7]] (mp/broadcast-like [[1 2] [3 4]] 7)))
  (is (m/equals [1 2 3] (mp/broadcast-like [2 3 4] [1 2 3])))
  (is (error? (mp/broadcast-like [1 2 3] [1 2]))))

;(deftest test-broadcast-coerce
;  (is (m/equals [2 2] (mp/broadcast-coerce [1 1] 2)))
;  (is (m/equals [2 2] (mp/broadcast-coerce [1 1] (double-array [2 2]))))
;  (is (m/equals [[7 7] [7 7]] (mp/broadcast-coerce [[1 2] [3 4]] 7)))
;  (is (m/equals [1 2 3] (mp/broadcast-coerce [2 3 4] [1 2 3])))
;  (is (error? (mp/broadcast-coerce [1 2 3] [1 2]))))
;
;(deftest test-divide
;  (is (== 2 (m/div 4 2)))
;  (is (op/== [2 1] (m/div [4 2] 2)))
;  (is (op/== [1 1.5] (m/div [2 3] 2)))
;  (is (m/equals [2 1] (m/div 4 [2 4])))
;  (is (m/equals [[1 2] [2 1]] (m/div [[4 8] [4 4]] [[4 4] [2 4]]))))
;
;(deftest test-pow-2
;  (is (== 8 (m/pow 2 3)))
;  (is (m/equals [0.5 2] (m/pow [2 0.5] -1))))
;
;(deftest test-logistic
;  (is (== 0.0 (m/logistic -10000)))
;  (is (== 0.5 (m/logistic 0)))
;  (is (== 1.0 (m/logistic 10000)))
;  (is (m/equals [0 0.5 1] (m/logistic [-10000 0 10000])))
;  (let [da (double-array [-10000 0 10000])]
;    (m/logistic! da)
;    (is (m/equals [0 0.5 1] da)))
;  (is (error? (m/logistic! 0.7))))
;
;(deftest test-addition
;  (testing "matrix addition"
;    (is (= [5.0] (m/add [3.0] [2.0])))
;    (is (= [[6.0]] (m/add [[2.0]] [[4.0]])))
;    (is (= [[[6.0]]] (m/add [[[2.0]]] [[[4.0]]])))))
;
;(deftest test-subtraction
;  (testing "unary subtraction"
;    (is (== (- 10) (op/- 10)))
;    (is (m/equals (m/sub [1 2]) (op/- [1 2]))))
;  (testing "matrix subtraction"
;    (is (= [1.0] (m/sub (m/array [3.0]) [2.0])))
;    (is (= [[8.0]] (m/sub (m/array [[12.0]]) [[4.0]])))
;    (is (= [[[8.0]]] (m/sub (m/array [[[12.0]]]) [[[4.0]]]))))
;  (testing "mutable sub"
;    (let [v (m/mutable [10 10])]
;      (m/sub! v [1 2] [1 2])
;      (is (m/equals [8 6] v))))
;  (testing "arity 3 sub regression"
;    (is (m/equals [-1 -2] (m/sub [1 2] [1 2] [1 2])))))
;
;(deftest test-transpose
;  (testing "transpose different dimensionalities"
;    (is (= 1 (m/transpose 1)))
;    (is (= [1.0] (m/transpose [1.0])))
;    (is (= [[1 3] [2 4]] (m/transpose [[1 2] [3 4]])))
;    (is (= [[1] [2] [3]] (m/transpose [[1 2 3]]))))
;  (testing "in place transpose"
;    (let [m [[1 2] [3 4]]]
;      (is (m/e= (m/transpose m) (m/transpose! (m/mutable m)))))))
;
;(deftest test-det
;  (testing "determinant"
;    (is (== 3 (m/det [[3]])))
;    (is (== -1 (m/det [[0 1] [1 0]])))))
;
;(deftest test-join
;  (is (m/equals [1 2 3] (m/join [1 2] 3)))
;  (is (m/equals [[1 1] [2 2] [3 3]] (m/join [[1 1]] [[2 2] [3 3]]))))
;
;(deftest test-join-along
;  (is (m/equals [1 2 3] (m/join-along 0 [1 2] [3])))
;  (is (m/equals [3 1 2] (m/join-along 0 [3] [1 2])))
;  (is (m/equals [[3 2 1 2] [1 2 3 4]] (m/join-along 1 [[3 2] [1 2]] [[1] [3]] [[2] [4]])))
;  (is (m/equals [[1 3 2 2] [3 1 2 4]] (m/join-along 1 [[1] [3]] [[3 2] [1 2]] [[2] [4]])))
;  (is (m/equals [[[1 2]]] (m/join-along 2 [[[1]]] [[[2]]]))))
;
;(deftest test-conjoin
;  (is (m/equals [1 2 3] (m/conjoin [1 2] 3)))
;  (is (m/equals [[1 1] [2 2] [3 3]] (m/conjoin [[1 1] [2 2]] [3 3])))
;  (is (m/equals [[1 1] [2 2] [3 3]] (m/conjoin [[1 1] [2 2]] 3)))
;  (is (error? (m/conjoin [[1 1] [2 2]] [3 3 3]))))
;
;(deftest test-conjoin-along
;  (is (m/equals [1 2 3] (m/conjoin-along 0 [1 2] 3)))
;  (is (m/equals [3 1 2] (m/conjoin-along 0 [3] [1] 2)))
;  (is (m/equals [[1 2] [3 4] [5 6]] (m/conjoin-along 0 [[1 2] [3 4]] [5 6])))
;  (is (m/equals [[1 2 5] [3 4 6]] (m/conjoin-along 1 [[1 2] [3 4]] [5 6]))))
;
;(deftest test-main-diagonal
;  (is (m/e== [1 2] (m/main-diagonal [[1 0] [4 2] [5 7]])))
;  (is (m/e== [1 4] (m/diagonal [[1 2] [3 4]]))))
;
;(deftest test-diagonals
;  (is (m/e== [1 4] (m/diagonal [[1 2] [3 4]] 0)))
;  (is (m/e== [2] (m/diagonal [[1 2] [3 4]] 1)))
;  (is (m/e== [3] (m/diagonal [[1 2] [3 4]] -1))))
;
;(deftest test-diagonal
;  (is (= [1 4] (m/diagonal [[1 2] [3 4] [5 6]])))
;  (is (= [] (m/diagonal [[1 2] [3 4] [5 6]]  8)))
;  (is (= [1 4] (m/diagonal [[1 2] [3 4] [5 6]]  0)))
;  (is (= [2]   (m/diagonal [[1 2] [3 4] [5 6]]  1)))
;  (is (= [3 6] (m/diagonal [[1 2] [3 4] [5 6]] -1)))
;  (is (= [5]   (m/diagonal [[1 2] [3 4] [5 6]] -2))))
;
;(deftest test-softplus
;  (is (m/equals [0.0 (m/log 2) 1000.0] (m/softplus [-1000.0 0.0 1000.0])))
;  (let [da (double-array [-1000 0 1000])]
;    (m/softplus! da)
;    (is (m/equals [0.0 (m/log 2) 1000.0] da))))
;
;(deftest test-relu
;  (is (m/equals [0.0 0.0 0.5 1000.0] (m/relu [-1000.0 0.0 0.5 1000.0])))
;  (let [da (double-array [-1000 0 1000])]
;    (m/relu! da)
;    (is (m/equals [0.0 0.0 1000.0] da))))
;
;(deftest test-softmax
;  (is (m/equals [0.5 0.5] (m/softmax [10 10])))
;  (is (m/equals [0.0 1.0] (m/softmax [-100 100]) 0.000001))
;  (let [da (double-array [-10 -10])]
;    (m/softmax! da)
;    (is (m/equals [0.5 0.5] da))))
;
;(deftest test-normalise
;  (testing "vector normalise"
;    (is (m/e== [1.0] (m/normalise (m/array [1.0]))))
;    (is (m/e== [1.0] (m/normalise (m/array [2.0]))))
;    (is (m/e== [-1.0 0.0] (m/normalise (m/array [-2.0 0.0]))))))
;
;(deftest test-mathsops
;  (testing "ops on scalars"
;    (is (== 1.0 (m/floor 1.2)))
;    (is (thrown? #? (:clj Throwable :cljs js/Error) (m/floor! 1.2))))
;  (testing "ops"
;    (is (= [1.0 2.0] (m/floor [1.2 2.7]))))
;  (testing "mutable maths ops"
;    (is (error? (m/signum! [1 2])))
;    (is (m/equals [1 0 1 -1] (m/signum! (double-array [1 0 2 -10]))))))
;
;(deftest test-scalar
;  (testing "special scalars"
;    (is (m/scalar? nil))
;    (is (not (m/scalar? [1])))
;    (is (not (m/scalar? (clojure.core.matrix.impl.wrappers/wrap-scalar 1)))))
;  (testing "numbers as scalars"
;    (is (m/scalar? 1))
;    (is (m/scalar? 1.0))
;    (is (m/scalar? 1/7)))
;  (testing "scalar dimensionality"
;    (is (== 0 (m/dimensionality 1.0)))
;    (is (== 0 (m/dimensionality :foo)))
;    (is (== 0 (m/dimensionality 'bar)))
;    (is (== 1.0 (m/mget 1.0)))
;    (is (nil? (m/shape 1.0))))
;  (testing "functional operations"
;    (is (= 2.0 (m/emap inc 1.0)))
;    (is (= 10.0 (m/emap + 4.0 6.0)))
;    (is (= 10.0 (m/emap + 1.0 2.0 3.0 4.0)))
;    (is (== 10.0 (m/ereduce #(+ %1 %2) 10.0)))
;    (is (== 3.0 (m/ereduce + 1.0 2.0)))
;    (is (= [1.0] (m/eseq 1.0))))
;  (testing "scalar operations"
;    (is (== 10 (m/inner-product 2 5)))
;    (is (== 10 (m/outer-product 2 5)))
;    (is (== 10 (m/scale 2 5)))
;    (is (== 10 (mp/pre-scale 2 5)))))
;
;(deftest test-vector-ops
;  (testing "vector dot product"
;    (is (== 1.0 (m/dot [1.0] [1.0])))
;    (is (== -1.0 (m/dot [1 2] [1 -1]))))
;  (testing "vector distance"
;    (is (== 1.0 (m/distance [0 0][0 1])))
;    (is (== 1.0 (m/distance [1 0][0 0])))))
;
;(deftest test-dimensions
;  (testing "vector dimensions"
;    (is (= 3 (m/row-count [1 2 3])))
;    (is (= 3 (m/row-count [[1 2] [2 3] [3 4]])))
;    (is (= 2 (m/column-count [[1 2] [2 3] [3 4]])))
;    (is (= [3 2] (m/shape [[1 2] [2 3] [3 4]])))
;    (is (= [2 2 2] (m/shape [[[1 2] [2 3]] [[3 4] [5 6]]]))))
;  (testing "element counts"
;    (is (== 1 (m/ecount :foo)))))
;
;(deftest test-broadcasting
;  (testing "broadcast shapes"
;    (is (nil? (broadcast-shape [1 2] [1 3])))
;    (is (= [2 2] (broadcast-shape [1 2] [2 1])))
;    (is (= [1 2 3] (broadcast-shape [1 2 3] [2 1])))
;    (is (= [1 2 3 4] (broadcast-shape [1 2 3 1] [2 1 4])))
;    (is (nil? (broadcast-shape [1 2 3 4] [2 3])))
;    (is (= [] (broadcast-shape [] [])))
;    (is (m/e= [[[nil]]] (m/broadcast nil [1 1 1]))))
;  (testing "broadcasted ops"
;    (is (m/e== [2 3] (m/add [1 2] 1.0)))
;    (is (m/e== [2 3] (m/add 1.0 [1 2])))
;    (is (m/e== [0 1] (m/sub [1 2] 1.0)))
;    (is (m/e== [0 -1] (m/sub 1.0 [1 2])))))
;
;(deftest test-sparsity
;  (testing "sparse?"
;    (is (not (m/sparse? [0 1 2]))))
;  (testing "density"
;    (is (== 0.75 (m/density [0 1 2 3])))))
;
;(deftest test-object-array
;  (is (m/e= [:a :b] (m/coerce [] (object-array [:a :b]))))
;  (let [a (m/to-object-array [1 2 3])]
;    (is (= 2 (aget a 1)))))
;
;(deftest test-permutation
;  (is (m/equals [[0 1] [1 0]] (m/permutation-matrix [1 0])))
;  (is (m/equals [[1 0] [0 1]] (m/permutation-matrix [0 1])))
;  (is (m/equals [[0 1 0] [0 0 1] [1 0 0]] (m/permutation-matrix [1 2 0]))))
;
;(deftest test-block-diagonal
;  (is (= [[1]] (m/block-diagonal-matrix [[[1]]])))
;  (is (= [[1 0.0] [0.0 2]] (m/block-diagonal-matrix [[[1]][[2]]])))
;  (is (= [[1 0.0 0.0] [0.0 2 3] [0.0 4 5]] (m/block-diagonal-matrix [[[1]][[2 3][4 5]]]))))
;
;(deftest test-non-zero-indices
;  (is (= []
;         (m/non-zero-indices [0])))
;  (is (= [0]
;         (m/non-zero-indices [1])))
;  (is (= [0 3 4]
;         (m/non-zero-indices [1 0 0 2 5 0])))
;  (is (= [[[0]] [[]] [[0]] [[]]]
;         (m/non-zero-indices [[[1.0]][[0]][[9.0]][[0]]])))
;  (is (= [[[1] [1]] [[0 1] [0]] [[0 1] [0 1]]]
;         (m/non-zero-indices [[[0.0 2.0][0 4.0]][[5.0 6.0][7.0 0]][[9.0 10.0][11.0 12.0]]]))))
;
;#?(:clj
;(deftest check-examples
;  (binding [*out* (StringWriter.)]
;    (testing "example code"
;      (clojure.core.matrix.demo.examples/all-examples))))
;)
;
;(deftest test-zeros
;  (is (m/zero-matrix? (m/zero-matrix 3 3)))
;  (is (m/zero-matrix? (m/zero-vector 3)))
;  (is (m/zero-matrix? (m/zero-array [2 2 2]))))
;
;(deftest test-numerical
;  (testing "numerical predicate"
;    (is true)
;    (is (clojure.core.matrix/numerical? 3))
;    (is (m/numerical? [1 1.0 2N (float 0)]))
;    (is (not (m/numerical? [1 :foo nil (float 0)])))
;    (is (not (m/numerical? nil)))))
;
;(deftest test-scalar-array
;  (let [a (m/scalar-array 3)]
;    (is (m/array? a))
;    (is (m/equals 3 a))
;    (is (m/equals 4 (m/mset a 4)))
;    (is (m/equals 6 (m/add a a)))
;    (is (= 3 (m/mget a)))
;    (is (= 3 (m/scalar a))))
;  (is (m/equals 0 (m/new-scalar-array))))
;
;(deftest test-min-max
;  (is (== 1 (m/emin [2 1 7])))
;  (is (== 7 (m/emax [2 1 7])))
;  (is (== 7 (m/emax [[4 3 2] [2 1 7] [-1 5 -20]])))
;  (is (m/equals [[2 5 2] [4 8 2] [5 6 3]] (m/clamp [[1 5 1] [4 10 2] [5 6 3]] 2 8))))
;
;(deftest test-predicates
;  (testing "scalar predicates"
;    (is (not (m/array? 1)))
;    (is (m/scalar? 1))
;    (is (m/scalar? (m/mget [1 2 3] 1)))
;    (is (m/scalar? (first (m/slices [1 2 3])))))
;  (testing "clojure vector predicates"
;    (is (m/array? [1 2]))
;    (is (m/vec? [1 2]))
;    (is (m/array? [[1 2] [3 4]]))
;    (is (m/matrix? [[1 2] [3 4]]))
;    (is (not (m/vec? [[1 2] [3 4]])))
;    (is (not (m/matrix? [[[1 2] [2 3]] [[3 4] [5 6]]]))))
;  (testing "row and column predicates"
;    (is (not (m/column-matrix? [1])))
;    (is (m/column-matrix? [[1]]))
;    (is (not (m/row-matrix? [1])))
;    (is (m/row-matrix? [[1]]))
;    (is (not (m/column-matrix? [1 2])))
;    (is (m/column-matrix? [[1] [2]]))
;    (is (not (m/column-matrix? [[1 2 3]])))
;    (is (m/row-matrix? [[1 2 3]]))
;    (is (not (m/row-matrix? [1 2]))))
;  (testing "mutability"
;    (is (not (m/mutable? [1 2])))
;    (is (m/mutable? (double-array [1 2]))))
;  (testing "symmetry"
;    (is (m/symmetric? (m/matrix [[1 -3][-3 2]])))
;    (is (not (m/symmetric? (m/matrix [[1 -3][-10 2]]))))
;    (is (m/symmetric? (m/matrix [[1 -4 -5][-4 2 -6][-5 -6 3]])))
;    (is (not (m/symmetric? (m/matrix [[1 -4 -5][-4 2 -6][-5 -10 3]]))))
;    (is (not (m/symmetric? (m/matrix [[1 2 3 4]]))))
;    (is (not (m/symmetric? (m/matrix [[1][2][3][4]]))))
;    (is (m/symmetric? (m/matrix [1 2 3 4])))
;    (is (m/symmetric? 2))
;    (is (m/symmetric? nil))
;    (is (m/symmetric? (double-array [1 2 3 4])))
;    (is (m/symmetric? (m/array [1 2 3 4])))
;    (is (m/symmetric? (m/array [[1 -3][-3 2]])))
;    (is (not (m/symmetric? (m/array [[1 -3][-10 2]]))))))
;
;(deftest test-inplace-operators
;  (is (op/== (m/matrix [5 7])
;             (op/+= (m/mutable (m/matrix [1 2]))
;                    (m/matrix [4 5]))))
;  (is (op/== (m/matrix [-4 6])
;             (op/-= (m/mutable (m/matrix [5 8]))
;                    (m/matrix [9 2]))))
;  (is (op/== (m/matrix [6 8])
;             (op/*= (m/mutable (m/matrix [3 2]))
;                    (m/matrix [2 4]))))
;  (is (op/== (m/matrix [2 0.5])
;             (op/div= (m/mutable (m/matrix [4 2]))
;                    (m/matrix [2 4])))))
;
;(deftest test-shift
;  (is (m/equals [1 2 3] (m/shift [1 2 3] [0])))
;  (is (m/equals [0 1 2] (m/shift [1 2 3] [-1])))
;  (is (m/equals [2 3 0] (m/shift [1 2 3] [1])))
;  (is (m/equals [0 0 0] (m/shift [1 2 3] [4])))
;  (is (m/equals [[4 0] [0 0]] (m/shift [[1 2] [3 4]] [1 1])))
;  (is (m/equals [0 0 0] (m/shift [1 2 3] [-5]))))
;
;(deftest test-norm
;  (is (= 30.0 (li/norm (m/matrix [[1 2][3 4]]))))
;  (is (= 30.0 (li/norm (m/matrix [[1 2][3 4]]) 2)))
;  (is (= 10.0 (li/norm (m/matrix [[1 2][3 4]]) 1)))
;  (is (= 100.0 (li/norm (m/matrix [[1 2][3 4]]) 3)))
;  (is (= 4 (li/norm (m/matrix [[1 2][3 4]]) Double/POSITIVE_INFINITY)))
;  (is (= 30.0 (li/norm (vector 1 2 3 4))))
;  (is (= 30.0 (li/norm (vector 1 2 3 4) 2)))
;  (is (= 10.0 (li/norm (vector 1 2 3 4) 1)))
;  (is (= 100.0 (li/norm (vector 1 2 3 4) 3)))
;  (is (= 4 (li/norm (vector 1 2 3 4) Double/POSITIVE_INFINITY))))
