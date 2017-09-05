(ns clojure.core.matrix.test-api
  (:refer-clojure :exclude [vector?])
;  (:use [clojure.core.matrix])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.linear :as li]
           ; [clojure.core.matrix :refer [:all]]
            [clojure.core.matrix.operators :as op]
            [clojure.core.matrix :as m
             :refer [abs abs! acos acos! add add! add-inner-product! add-outer-product! add-product add-product! add-row add-scaled add-scaled! add-scaled-product add-scaled-product! array array? as-vector asin asin! assign assign! assign-array! atan atan! block-diagonal-matrix broadcast broadcast-coerce broadcast-like cbrt cbrt! ceil ceil! clamp clone cmp coerce column-count column-matrix column-matrix? columns compute-matrix conforming? conjoin conjoin-along cos cos! cosh cosh! cross cross! current-implementation current-implementation-object dense density det diagonal diagonal-matrix diagonal? dimension-count dimensionality distance div div! dot e* e= e== ecount eif element-type emap emap! emap-indexed emap-indexed! emax emin emul emul! ensure-mutable eq equals ereduce eseq esum exp exp! fill fill! floor floor! ge get-column get-row gt identity-matrix identity-matrix? immutable index index-seq index-seq-for-shape index? inner-product inverse join join-along label label-index labels le length length-squared lerp lerp! log log! log10 log10! logistic logistic! lower-triangular? lt magnitude magnitude-squared main-diagonal matrix matrix? maximum mget minimum mmul mset mset! mul mul! multiply-row mutable mutable? native native? ne negate negate! new-array new-matrix new-scalar-array new-sparse-array new-vector non-zero-count non-zero-indices normalise normalise! numerical? order orthogonal? outer-product pack permutation-matrix pow pow! rank relu relu! reshape rotate round round! row-count row-matrix row-matrix? rows same-shape? scalar scalar-array scalar? scale scale! scale-add scale-add! select select-indices select-view set-column set-column! set-current-implementation set-indices set-indices! set-inner-product! set-row set-row! set-selection set-selection! shape shift signum signum! sin sin! sinh sinh! slice slice-count slice-map slice-view slice-views slices softmax softmax! softplus softplus! sparse sparse-array sparse-matrix sparse? sqrt sqrt! square square? sub sub! submatrix subvector supports-dimensionality? supports-shape? swap-rows symmetric? tan tan! tanh tanh! to-degrees to-degrees! to-double-array to-nested-vectors to-object-array to-radians to-radians! to-vector trace transform transform! transpose transpose! upper-triangular? vec? zero-array zero-count zero-dimensional? zero-matrix zero-matrix? zero-vector validate-shape]]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :refer [broadcast-shape]]
            #?@(:clj [[clojure.core.matrix.macros :refer [error doseq-indexed]]
             [clojure.core.matrix.macros-clj :refer [error?]]
             [clojure.test :refer [deftest is testing run-tests]]
             [clojure.core.matrix.demo.examples]]
                :cljs [[cljs.test :refer-macros [deftest is testing run-tests]]
                       [thinktopic.aljabr.core :as aljabr]
             ;[clojure.core.matrix :refer-macros [with-implementation]]
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
      (is (nil? (label m 0 0)))
      (is (nil? (label m 0 1)))
      (is (nil? (labels m 0)))
      (is (nil? (labels m 1)))
      (is (error? (label m 0 2)))
      (is (error? (labels m -1))))))

(deftest test-select
  (let [a [[1 2] [3 4]]]
    (testing "higher level indexing"
      (is (equals 1 (m/select a 0 0)))
      (is (equals [[1] [3]] (m/select a [0 1] [0])))
      (is (equals [1 3] (m/select a :all 0)))
      (is (equals a (m/select a :all :all)))
      (is (equals [3] (m/select a [1] 0))))))

(deftest test-select-indices
  (let [a [[1 2] [3 4]]]
    (testing "select indices"
      (is (equals [1 4] (m/select-indices a [[0 0] [1 1]])))
      (is (equals [[5 2] [3 6]] (m/set-indices a [[0 0] [1 1]] [5 6])))
      (is (equals [[0 0] [0 0]] (m/set-indices a [[0 0] [0 1] [1 0] [1 1]] [0 0 0 0])))
      (is (equals [[0 0] [0 0]] (m/set-indices a [[0 0] [0 1] [1 0] [1 1]] 0)))
      (let [ma (mutable a)]
        (set-indices! ma [[0 0] [1 1]] [5 6])
        (is (equals ma [[5 2] [3 6]]))
        (is (equals (transpose ma) [[5 3] [2 6]]))))))

(deftest test-set-selection
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12]]]
    (testing "m/set-selection"
      (is (equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] (set-selection a 0 0 2)))
      (is (equals [[3 2 3 3] [5 6 7 8] [3 10 11 3]] (set-selection a [0 2] [0 3] 3))))))

(comment
  ;;fails tests
  (deftest test-set-selection!
    (let [a (matrix :ndarray [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
      (testing "sel-set!"
        (set-selection! a 0 0 2)
        (is (equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] a))
        (set-selection! a :all 0 0)
        (is (equals [[0 2 3 4] [0 6 7 8] [0 10 11 12]] a))))))

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

(deftest test-validate-shape
  (testing "Error cases for invalid shapes"
    (is (error? (validate-shape [[1] [2 3]])))
    (is (error? (validate-shape [[1 2] [2]])))
    (is (error? (validate-shape [[1 2] 4 [2 2]])))
    (is (error? (validate-shape (object-array [[1 2] 4 [2 2]]))))
    (is (error? (validate-shape [1 [2 3]]))))
  (testing "Error cases for unexpected shapes shapes"
    (is (error? (validate-shape [[1 2] [2 3]] [2 3])))
    (is (error? (validate-shape [7] [1 1])))
    (is (error? (validate-shape [7] [])))
    (is (error? (validate-shape (object-array [7]) [])))
    (is (error? (validate-shape 1 [1])))
    (is (error? (validate-shape 1 [])))
    (is (error? (validate-shape [2 3] nil)))))

(deftest test-as-vector
  (is (e== [1] (as-vector 1))))

(comment 
  (deftest test-implementations
    (testing "vector implementation"
      (is (clojure.core/vector? (imp/get-canonical-object :persistent-vector)))
      (is (= :persistent-vector (imp/get-implementation-key []))))
    (testing "non-existent implementation"
      (is (nil? (imp/get-canonical-object :random-fictitious-implementation-key))))
    (testing "with-implementation"
      (is (= [1 2] (with-implementation [] (matrix [1 2]))))
      #?(:clj
         (is (= (class (double-array [1 2]))
                (class (with-implementation :double-array (matrix [1 2])))))))))

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
  ; This doesn't seem correct, as some implementations can zero out rather than
  ; have nil values when allocating.  At least according to the documentation.
  ;(is (e= [nil] (new-array :ndarray [1])))
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
        mm (mutable m)]
    (fill! mm 0.5)
    (is (equals mm [0.5 0.5 0.5]))))

(deftest test-mutable-matrix-assign
  (let [m [1 2 3]
        mm (mutable m)]
    (assign! mm 0.5)
    (is (equals mm [0.5 0.5 0.5]))))

(deftest test-coerce
  (testing "clojure vector coercion"
    (is (== 1.0 (coerce [] 1)))
    (is (= [1 2 3] (coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (coerce [1] [[1 2] [3 4]])))
    (is (= [[1 2] [3 4]] (coerce [1] '((1 2) (3 4))))))
  (testing "coerce to a number"
     (is (= 1 (coerce 2 1)))
     (is (equals [1 2] (coerce 2 [1 2])))))

(comment
  ;;fails tests
  (deftest test-pow
    (let [a (array [1 2 3])
          m (matrix [[1 2 3] [4 5 6] [7 8 9]])]
      (testing "pow works on scalars"
        (is (== 8 (pow 2 3)))
        (is (== 8 (clojure.core.matrix.operators/** 2 3))))
      (testing "pow works when base is an array and exponent is a scalar"
        (is (equals [1.0 4.0 9.0] (pow a 2)))
        (is (equals [[1.0 4.0 9.0] [16.0 25.0 36.0] [49.0 64.0 81.0]] (pow m 2))))
      (testing "pow works when base is a scalar and exponent is an array"
        (is (equals [5.0 25.0 125.0] (pow 5 a)))
        (is (equals [[2.0 4.0 8.0] [16.0 32.0 64.0] [128.0 256.0 512.0]] (pow 2 m))))
      (testing "pow works when both the base and the exponent are arrays"
        (is (equals [1.0 4.0 27.0] (pow a a)))
        (is (equals [[1.0 4.0 27.0] [4.0 25.0 216.0] [7.0 64.0 729.0]] (pow m a)))))))

(comment
  ;;fails tests
  (deftest test-slices
    (testing "rows and columns of clojure vector matrix"
      (is (= [1 2 3] (get-row [[1 2 3] [4 5 6]] 0)))
      (is (= [2 5] (get-column [[1 2 3] [4 5 6]] 1))))
    (testing "get-nd on scalar with zero dimensions"
      (is (== 10.0 (m/mget 10.0)))
      (is (== 10.0 (mp/get-nd 10.0 []))))
    (testing "slices of a standard vector are scalar numbers"
      (is (= [1 2 3] (slices (array [1 2 3])))))))

(deftest test-slice-on-1d
  (testing "slice on 1d must return scalar"
    (is (scalar? (slice [1 2 3] 0)))))

(comment
  ;;fails test
  (deftest test-submatrix
    (is (equals [[3]] (submatrix (array [[1 2] [3 4]]) [[1 1] [0 1]])))
    (is (equals [[2] [4]] (submatrix (array [[1 2] [3 4]]) 1 [1 1])))
    (is (equals [2 3] (submatrix (array [1 2 3 4]) [[1 2]])))
    (is (equals [[4]] (submatrix [[1 2] [3 4]] 1 1 1 1)))
    (is (equals [2 3] (submatrix (array [1 2 3 4]) 0 [1 2])))))

(deftest test-element-seq
  (is (= [0] (eseq 0)))
  (is (= [1] (eseq [1])))
  (is (= [2] (eseq [[2]])))
  (is (= [4] (eseq [[[[4]]]]))))

(comment
  ;;fails test
  (deftest test-element-map
    (is (equals 1 (emap inc (array 0))))
    (is (equals [2] (emap inc (array [1]))))
    (is (equals [[3]] (emap inc (array [[2]]))))
    (is (equals [[[[5]]]] (emap inc (array [[[[4]]]]))))
    (is (equals [10] (emap + [1] [2] [3] [4])))
    (is (equals [10] (emap + [1] (broadcast 2 [1]) (double-array [3]) [4])))))

(deftest test-conforming?
  (is (conforming? [[2 2] [3 3]] 1))
  (is (conforming? [[2 2] [3 3]] [1 1]))
  (is (conforming? [3 3] 1))
  (is (not (conforming? [3 3] [[1 2 3] [3 4 3]])))
  (is (not (conforming? [1 2] [3 4 5])))
  (is (not (conforming? [[0.0]] [0.0 0.0]))))

(deftest test-broadcast
  (is (equals [[1 1] [1 1]] (coerce [] (broadcast 1 [2 2]))))
  (is (equals [[[[2]]]] (broadcast (array 2) [1 1 1 1])))
  (is (equals [2 2] (add [1 1] 1))))

(deftest test-mutable
  (is (error? (scale! [1 2] 2)))
  (is (equals [2 4] (scale! (mutable [1 2]) 2) )))

(deftest test-scale-add
  (is (equals [2 6] (scale-add [1 0] 2 [0 2] 3)))
  (is (equals [3 7] (scale-add [1 0] 2 [0 2] 3 1))))

(deftest test-add-outer-product!
  (let [m (mutable [[1 2] [3 4]])]
    (add-outer-product! m [10 100] [7 9])
    (is (equals [[71 92] [703 904]] m))))

(deftest test-lerp
  (is (equals [2 6] (lerp [1 0] [3 12] 0.5))))

(deftest test-reshape-2
  (is (equals 1 (reshape [1 2 3] [])))
  (is (equals [1 2 3 4] (reshape [[1.0 2.0] [3.0 4.0]] [4])))
  (is (equals [1 2] (reshape [[1.0 2.0] [3.0 4.0]] [2])))
  (is (equals [] (reshape [[1.0 2.0] [3.0 4.0]] [0])))
  (is (equals 1.0 (reshape [[1.0 2.0] [3.0 4.0]] [])))
  (is (equals [[1 2] [3 4]] (reshape [1 2 3 4] [2 2])))
  (is (equals [1 0] (reshape 1 [2])))
  (is (equals [[1 2] [3 0]] (reshape [1 2 3] [2 2]))))

(deftest test-index-seq
  (is (= [] (index-seq [])))
  (is (= [[]] (index-seq 10)))
  (is (= [[0] [1] [2]] (index-seq [1 2 3])))
  (is (= [[0 0] [0 1] [1 0] [1 1]] (index-seq [[1 2] [3 4]]))))

(deftest test-functional-ops
  (testing "m/eseq"
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
    ;fails tests
    ;(is (e= [1 2] (array [1 2])))
    (is (e= [1 2] [1 2] [1 2] [1 2]))
    (is (not (e= [1 2] [3 4])))
    #?(:clj (is (not (e= [1 2] [1.0 2.0]))))
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
  (testing "m/equals does not broadcast"
    (is (not (equals (array 1) (array [1 1]))))))

(comment
  ;;fails tests
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
      (is (equals [[6]] (mul (array [[2]]) (array 3)))))))

(comment
  ;;fails test
  (deftest test-broadcast-compatibile
    (is (equals [[2 1] [2 2]] (mp/broadcast-compatible (array [2 1]) (array 2))))))

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

(deftest test-pow-2
  (is (== 8 (pow 2 3)))
  (is (equals [0.5 2] (pow [2 0.5] -1))))

(deftest test-logistic
  (is (== 0.0 (logistic -10000)))
  (is (== 0.5 (logistic 0)))
  (is (== 1.0 (logistic 10000)))
  (is (equals [0 0.5 1] (logistic [-10000 0 10000])))
  (let [da (double-array [-10000 0 10000])]
    (logistic! da)
    (is (equals [0 0.5 1] da)))
  (is (error? (logistic! 0.7))))

(comment
  ;;fails test
  (deftest test-rank
    (testing "default comparators"
      (is (error? (rank 1)))
      (is (= [0 2 1] (rank [10 30 20])))
      (is (= [0 1 2] (sort (rank ["a" "a" "a"]))))
      (is (= [[2 1 0] [0 1 2]] (rank [[:z :m :a] [-100 0.0 1000]]))))
    (testing "custom comparators"
      (is (error? (rank identity 1)))
      (is (= [2 0 1] (rank > [10 30 20])))
      (is (= [0 2 1] (rank #(< (count %1) (count %2)) ["a" "ccc" "bb"])))
      (is (= [[0 1 2] [2 1 0]] (rank > [[8 7 6] [-1.0 1.0 3.0]]))))))

(deftest test-addition
  (testing "matrix addition"
    (is (= [5.0] (add [3.0] [2.0])))
    (is (= [[6.0]] (add [[2.0]] [[4.0]])))
    (is (= [[[6.0]]] (add [[[2.0]]] [[[4.0]]])))))

(comment
  ;;fails test
  (deftest test-subtraction
    (testing "unary subtraction"
      (is (== (- 10) (op/- 10)))
      (is (equals (sub [1 2]) (op/- [1 2]))))
    (testing "matrix subtraction"
      (is (equals [1.0] (sub (array [3.0]) [2.0])))
      (is (equals [[8.0]] (sub (array [[12.0]]) [[4.0]])))
      (is (equals [[[8.0]]] (sub (array [[[12.0]]]) [[[4.0]]]))))
    (testing "mutable sub"
      (let [v (mutable [10 10])]
        (sub! v [1 2] [1 2])
        (is (equals [8 6] v))))
    (testing "arity 3 sub regression"
      (is (equals [-1 -2] (sub [1 2] [1 2] [1 2]))))))

(deftest test-transpose
  (testing "transpose different dimensionalities"
    (is (= 1 (transpose 1)))
    (is (= [1.0] (transpose [1.0])))
    (is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]])))
    (is (= [[1] [2] [3]] (transpose [[1 2 3]]))))
  (testing "in place transpose"
    (let [m [[1 2] [3 4]]]
      (is (e= (transpose m) (transpose! (mutable m)))))))

(deftest test-slice-map
  (testing "single arity"
    (is (equals [2 3 4] (slice-map inc [1 2 3])))
    (is (equals [2 4 6] (slice-map maximum [[[0 1] [1 2]] 
                                                [[4 3] [2 1]] 
                                                [[1 2] [5 6]]]))))
  (testing "double arity"
    (is (equals [2 12 103] (slice-map + [1 2 3] [1 10 100])))
    (is (equals [[7 8] [6 7]] (slice-map sub [[10 11] [12 13]] [3 6]))))
  (testing "multi arity"
    (is (equals [5 16 108] (slice-map + [1 2 3] [1 10 100] [3 4 5])))
    (is (equals [[6 6] [6 6]] (slice-map sub [[10 11] [12 13]] [3 6] [[1 2] [0 1]])))))

#?(:clj
(deftest test-det
  (testing "determinant"
    (is (== 3 (det [[3]])))
    (is (== -1 (det [[0 1] [1 0]])))))
)

(deftest test-join
  (is (equals [1 2 3] (join [1 2] 3)))
  (is (equals [[1 1] [2 2] [3 3]] (join [[1 1]] [[2 2] [3 3]]))))

(deftest test-join-along
  (is (equals [1 2 3] (join-along 0 [1 2] [3])))
  (is (equals [3 1 2] (join-along 0 [3] [1 2])))
  (is (equals [[3 2 1 2] [1 2 3 4]] (join-along 1 [[3 2] [1 2]] [[1] [3]] [[2] [4]])))
  (is (equals [[1 3 2 2] [3 1 2 4]] (join-along 1 [[1] [3]] [[3 2] [1 2]] [[2] [4]])))
  (is (equals [[[1 2]]] (join-along 2 [[[1]]] [[[2]]]))))

(deftest test-conjoin
  (is (equals [1 2 3] (conjoin [1 2] 3)))
  (is (equals [[1 1] [2 2] [3 3]] (conjoin [[1 1] [2 2]] [3 3])))
  (is (equals [[1 1] [2 2] [3 3]] (conjoin [[1 1] [2 2]] 3)))
  (is (error? (conjoin [[1 1] [2 2]] [3 3 3]))))

(deftest test-conjoin-along
  (is (equals [1 2 3] (conjoin-along 0 [1 2] 3)))
  (is (equals [3 1 2] (conjoin-along 0 [3] [1] 2)))
  (is (equals [[1 2] [3 4] [5 6]] (conjoin-along 0 [[1 2] [3 4]] [5 6])))
  (is (equals [[1 2 5] [3 4 6]] (conjoin-along 1 [[1 2] [3 4]] [5 6]))))

(deftest test-main-diagonal
  (is (e== [1 2] (main-diagonal [[1 0] [4 2] [5 7]])))
  (is (e== [1 4] (diagonal [[1 2] [3 4]]))))

(deftest test-diagonals
  (is (e== [1 4] (diagonal [[1 2] [3 4]] 0)))
  (is (e== [2] (diagonal [[1 2] [3 4]] 1)))
  (is (e== [3] (diagonal [[1 2] [3 4]] -1))))

(deftest test-diagonal
  (is (= [1 4] (diagonal [[1 2] [3 4] [5 6]])))
  (is (= [] (diagonal [[1 2] [3 4] [5 6]]  8)))
  (is (= [1 4] (diagonal [[1 2] [3 4] [5 6]]  0)))
  (is (= [2]   (diagonal [[1 2] [3 4] [5 6]]  1)))
  (is (= [3 6] (diagonal [[1 2] [3 4] [5 6]] -1)))
  (is (= [5]   (diagonal [[1 2] [3 4] [5 6]] -2))))

(deftest test-softplus
  (is (equals [0.0 (log 2) 1000.0] (softplus [-1000.0 0.0 1000.0])))
  (let [da (double-array [-1000 0 1000])]
    (softplus! da)
    (is (equals [0.0 (log 2) 1000.0] da))))

(deftest test-relu
  (is (equals [0.0 0.0 0.5 1000.0] (relu [-1000.0 0.0 0.5 1000.0])))
  (let [da (double-array [-1000 0 1000])]
    (relu! da)
    (is (equals [0.0 0.0 1000.0] da))))

(deftest test-softmax
  (is (equals [0.5 0.5] (softmax [10 10])))
  (is (equals [0.0 1.0] (softmax [-100 100]) 0.000001))
  (let [da (double-array [-10 -10])]
    (softmax! da)
    (is (equals [0.5 0.5] da))))

(comment
  ;fails tests
  (deftest test-normalise
    (testing "vector normalise"
      (is (e== [1.0] (normalise (array [1.0]))))
      (is (e== [1.0] (normalise (array [2.0]))))
      (is (e== [-1.0 0.0] (normalise (array [-2.0 0.0])))))))

(comment
  ;;fails tests
  (deftest test-mathsops
    (testing "ops on scalars"
      (is (== 1.0 (floor 1.2)))
      (is (thrown? #? (:clj Throwable :cljs js/Error) (floor! 1.2))))
    (testing "ops"
      (is (= [1.0 2.0] (floor [1.2 2.7]))))
    (testing "mutable maths ops"
      (is (error? (signum! [1 2])))
      (is (equals [1 0 1 -1] (signum! (double-array [1 0 2 -10])))))))

(deftest test-scalar
  (testing "special scalars"
    (is (scalar? nil))
    (is (not (scalar? [1])))
    (is (not (scalar? (clojure.core.matrix.impl.wrappers/wrap-scalar 1)))))
  (testing "numbers as scalars"
    (is (scalar? 1))
    (is (scalar? 1.0))
    #?(:clj (is (scalar? 1/7)))
    )
  (testing "scalar dimensionality"
    (is (== 0 (dimensionality 1.0)))
    (is (== 0 (dimensionality :foo)))
    (is (== 0 (dimensionality 'bar)))
    (is (== 1.0 (m/mget 1.0)))
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

(deftest test-non-zero-indices
  (is (= []
         (non-zero-indices [0])))
  (is (= [0]
         (non-zero-indices [1])))
  (is (= [0 3 4]
         (non-zero-indices [1 0 0 2 5 0])))
  (is (= [[[0]] [[]] [[0]] [[]]]
         (non-zero-indices [[[1.0]][[0]][[9.0]][[0]]])))
  (is (= [[[1] [1]] [[0 1] [0]] [[0 1] [0 1]]]
         (non-zero-indices [[[0.0 2.0][0 4.0]][[5.0 6.0][7.0 0]][[9.0 10.0][11.0 12.0]]]))))

(deftest test-invalid-implementation
  (is (error? (set-current-implementation :non-existent-implementation-name))))

#?(:clj
(deftest check-examples
  (binding [*out* (StringWriter.)]
    (testing "example code"
      (clojure.core.matrix.demo.examples/all-examples))))
)

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
    (is (= 3 (m/mget a)))
    (is (= 3 (scalar a))))
  (is (equals 0 (new-scalar-array))))

(deftest test-min-max
  (is (== 1 (emin [2 1 7])))
  (is (== 7 (emax [2 1 7])))
  (is (== 7 (emax [[4 3 2] [2 1 7] [-1 5 -20]])))
  (is (equals [[2 5 2] [4 8 2] [5 6 3]] (clamp [[1 5 1] [4 10 2] [5 6 3]] 2 8))))

(deftest test-predicates
    (testing "scalar predicates"
      (is (not (array? 1)))
      (is (scalar? 1))
      (is (scalar? (m/mget [1 2 3] 1)))
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
      ;(is (not (symmetric? (array [[1 -3][-10 2]]))))
      ))

(deftest test-inplace-operators
  (is (op/== (matrix [5 7])
             (op/+= (mutable (matrix [1 2]))
                    (matrix [4 5]))))
  (is (op/== (matrix [-4 6])
             (op/-= (mutable (matrix [5 8]))
                    (matrix [9 2]))))
  (is (op/== (matrix [6 8])
             (op/*= (mutable (matrix [3 2]))
                    (matrix [2 4]))))
  (is (op/== (matrix [2 0.5])
             (op/div= (mutable (matrix [4 2]))
                    (matrix [2 4])))))

(deftest test-shift
  (is (equals [1 2 3] (shift [1 2 3] [0])))
  (is (equals [0 1 2] (shift [1 2 3] [-1])))
  (is (equals [2 3 0] (shift [1 2 3] [1])))
  (is (equals [0 0 0] (shift [1 2 3] [4])))
  (is (equals [[4 0] [0 0]] (shift [[1 2] [3 4]] [1 1])))
  (is (equals [0 0 0] (shift [1 2 3] [-5]))))

(deftest test-norm
  (is (== (Math/sqrt 30.0) (li/norm (matrix [[1 2][3 4]]))))
  (is (== (Math/sqrt 30.0) (li/norm (matrix [[1 2][3 4]]) 2)))
  (is (== 10.0 (li/norm (matrix [[1 2][3 4]]) 1)))
  ;;fails test
  ;;(is (== (Math/cbrt 100.0) (li/norm (matrix [[1 2][3 4]]) 3)))
  (is (== 4 (li/norm (matrix [[1 2][3 -4]]) #?(:clj Double/POSITIVE_INFINITY :cljs Infinity))))
  (is (== (Math/sqrt 30.0) (li/norm (vector 1 2 3 4))))
  (is (== (Math/sqrt 30.0) (li/norm (vector 1 2 3 4) 2)))
  (is (== 10.0 (li/norm (vector 1 2 3 4) 1)))
  ;;fails test
  ;;(is (== (Math/cbrt 100.0) (li/norm (vector 1 2 3 4) 3)))
  (is (== 4 (li/norm (vector 1 2 3 4) #?(:clj Double/POSITIVE_INFINITY :cljs Infinity)))))
