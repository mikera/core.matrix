(ns clojure.core.matrix.compliance-tester
  (:require
    [clojure.core.matrix.operators :as ops]
    [clojure.core.matrix.protocols :as mp]
    [clojure.core.matrix.generic :as generic]
    [clojure.core.matrix.implementations :as imp]
    [clojure.core.matrix.impl.persistent-vector :as pvector]
    [clojure.core.matrix.utils :as u]
    [clojure.core.matrix.linear :as linear]
    [clojure.core.matrix.macros #?(:clj :refer :cljs :refer-macros) [error]]
    [clojure.core.matrix :as mat])
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.core.matrix.macros-clj :refer [error?]])
     :cljs (:require-macros
             [cljs.test :refer [deftest is testing]]
             [clojure.core.matrix.macros-cljs :refer [error?]])))

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
    (not (mat/mutable? m))
    (let [clonem (mat/clone m)]
      (mutable-fn clonem)
      (mat/equals clonem (immutable-fn m)))))


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
      create-dimensioned
      (filter #(mat/supports-dimensionality? m %)
              (range 1 5)))))

;; ===========================================
;; General implementation tests

(defn test-impl-scalar-array
  [im]
  (let [sa (mat/new-scalar-array im)]
    (is (mat/array? sa))
    (is (mat/zero-dimensional? sa))))

(defn test-implementation-key
  [im]
  (testing "Implementation keyword"
    (is (keyword? (imp/get-implementation-key im)))
    (is (= (imp/get-implementation-key im) (imp/get-implementation-key (imp/get-canonical-object im))))))

(defn test-implementation
  "Tests that an implementation conforms to any general requirements"
  ([im]
    (test-impl-scalar-array im)
    (test-implementation-key im)))

;; ==============================================
;; Test general array assumprions
;;
;; should work on any array with Number elements

(defn test-double-array-ops [m]
  (let [aa (mat/to-double-array m true)  ;; a new copy
        ta (mat/to-double-array m)       ;; an array, may or may not be a new copy
        ca (mat/to-double-array m false) ;; not a copy, i.e. internal array or nil
        c (mat/ecount m)]
    (is (= c (count aa) (count ta)))
    (when ca
      (is (= c (count ca)))
      (is (= (seq ta) (seq ca))))
    (is (= (seq ta) (map double (mat/eseq m))))))

(defn test-dimensionality-assumptions [m]
  (testing "mat/shape"
    (let [sh (mat/shape m)
          dims (mat/dimensionality m)]
      (is (u/valid-shape? sh))
      (is (== (count sh) dims))
      (is (= sh (mat/validate-shape m)))
      (is (= sh (mat/validate-shape m sh)))
      (is (= (seq sh) ;; we need the seqs to account for empty mat/shapes (need to comapre equal to nil)
             (seq (for [i (range dims)] (mat/dimension-count m i)))))))
  (testing "vectors always have dimensionality == 1"
    (is (or (= (boolean (mat/vec? m)) (boolean (== 1 (mat/dimensionality m)))) (error "Failed with : " m))))
  (testing "scalars always have dimensionality == 0"
    (is (or (not (mat/scalar? m)) (== 0 (mat/dimensionality m)))))
  (testing "zero-dimensionality"
    (is (= (mat/zero-dimensional? m) (== 0 (mat/dimensionality m)))))
  (testing "element count"
    (is (== 0 (mat/ecount (mat/new-array [0]))))
    (is (== (mat/ecount m) (reduce * 1 (mat/shape m))))
    (is (== (mat/ecount m) (mat/ereduce (fn [acc _] (inc acc)) 0 m)))
    (is (== (mat/ecount m) (reduce (fn [acc _] (inc acc)) 0 (mat/eseq m))))
    (is (or (not (mat/scalar? m)) (== 1 (mat/ecount m)))))
  (testing "accessing outside existing dimensions is an error"
    (let [sh (mat/shape m)
          dims (count sh)]
      (is (thrown? #?(:clj Throwable :cljs js/Error) (mat/dimension-count m -1)))
      (is (thrown? #?(:clj Throwable :cljs js/Error) (mat/dimension-count m dims))))))

(defn test-immutable-assumptions [m]
  (testing "immutable coerce"
    (let [im (mat/immutable m)]
      (is (not (mat/mutable? im)))
      (is (mat/e= m im)))))

(defn test-mutable-assumptions [m]
  (testing "ensure mutable"
    (let [em (mat/ensure-mutable m)]
      (is (mat/mutable? em))
      (is (mat/e= m em))))
  (testing "mutable-matrix works ok"
    (let [mm (mat/mutable m)]
      (is (mat/mutable? mm))
      (is (not (identical? m mm)))
      (is (mat/e= mm m))))
  (testing "modifying a cloned mutable array does not modify the original"
    (when (and (mat/mutable? m) (> 0 (mat/ecount m)))
            (let [cm (mat/clone m)
                  ix (first (mat/index-seq m))
                  v1 (first (mat/eseq m))
                  v2 (if (and (number? v1) (== v1 0)) 1 0)]
              (apply mat/mset! cm (concat ix [v2]))
              (is (mat/equals v1 (apply mat/mget m ix)))
              (is (mat/equals v2 (apply mat/mget cm ix))))))
  (testing "attempt to modify an immutable array results in an exception"
    (when-not (mat/mutable? m)
      (let [cm (mat/clone m)
            ix (first (mat/index-seq m))
            v1 (first (mat/eseq m))
            v2 (if (and (number? v1) (== v1 0)) 1 0)]
        (is (thrown? #?(:clj Throwable :cljs js/Error) (apply mat/mset! cm (concat ix [v2]))))))))

(defn test-reshape [m]
  (let [eq (if (mat/numerical? m) mat/equals mat/e=)
        c (mat/ecount m)]
    (when (pos? c)
      (when (mat/supports-dimensionality? m 1)
        (is (eq (mat/eseq m) (mat/reshape m [c]))))
      (when (mat/supports-dimensionality? m 2)
        (is (eq (mat/eseq m) (mat/eseq (mat/reshape m [1 c]))))
        (is (eq (mat/eseq m) (mat/eseq (mat/reshape m [c 1]))))))))

(defn test-broadcast [m]
  (let [c (mat/ecount m)]
    (when (pos? c)
      (is (mat/e= m (first (mat/slices (mat/broadcast m (cons 2 (mat/shape m)))))))
      (is (== c (mat/ecount (mat/broadcast m (cons 1 (mat/shape m))))))
      (is (== (* 3 c) (mat/ecount (mat/broadcast m (cons 3 (mat/shape m)))))))))

(defn test-slice-assumptions [m]
  (let [dims (mat/dimensionality m)]
    (when (pos? dims) ;; slices only valid for dimensionality 1 or above
      (let [slcs (mat/slices m)]
        (doseq [sl slcs]
          (is (== (dec dims) (mat/dimensionality sl)))
          (is (= (next (mat/shape m)) (seq (mat/shape sl)))))
        (when (> dims 1) ;; we get non-mutable scalars back when slicing 1d
          (if-let [ss (seq slcs)]
            (let [fss (first ss)]
              (is (= (mat/mutable? fss) (mat/mutable? m)))))
          (is (mat/e= m slcs)))))))

(defn test-slice-returns-scalar-on-1d [m]
  (when (and (= 1 (mat/dimensionality m)) (pos? (mat/ecount m)))
    (is (mat/scalar? (mat/slice m 0)))))

(defn test-submatrix-assumptions [m]
  (let [shp (mat/shape m)
        dims (mat/dimensionality m)
        full-ranges (map (fn [c] [0 c]) shp)]
    (is (mat/e= m (mat/submatrix m full-ranges)))
    (is (mat/e= m (mat/submatrix m (repeat dims nil))))
    ;; TODO: test a variety of different submatrices
    ))

(defn test-general-transpose [m]
  (when (pos? (mat/ecount m))
    (let [mt (mat/transpose m)]
      (is (mat/e= m (mat/transpose mt)))
      (is (= (seq (mat/shape m))
             (seq (reverse (mat/shape mt))))))))

(defn test-rotate [m]
  (let [sh (mat/shape m)
        dims (mat/dimensionality m)]
    (is (mat/e= m (mat/rotate m sh)))
    (when (>= dims 1)
      (is (mat/e= m (mat/rotate m 0 (sh 0))))
      (is (mat/e= (mat/rotate m 0 (inc (sh 0))) (mat/rotate m 0 1))))
    (is (mat/e= m (mat/rotate (mat/rotate m 0 1) 0 -1)))
    (is (mat/e= m (mat/rotate (mat/rotate m 1 1) 1 -1)))))

(defn test-coerce [m]
  ;; (is (identical? m (mat/coerce m m))) ;; TODO: figure out if we should enforce this?
  (let [vm (mp/convert-to-nested-vectors m)]
    (is (or (clojure.core/vector? vm) (== 0 (mp/dimensionality vm))))
    (is (pvector/is-nested-persistent-vectors? vm))
    (is (mat/e= m vm))))

(defn test-pack [m]
  (is (mat/e= m (mat/pack m))))

(defn test-vector-round-trip [m]
  (is (mat/e= m (mat/coerce m (mat/coerce [] m)))))

(defn test-ndarray-round-trip [m]
  ;; TODO: reinstate once NDArray startup time fixed
  ;; (is (mat/e= m (mat/coerce m (mat/coerce :ndarray m))))
  )

(defn test-as-vector [m]
  (when-let [av (mat/as-vector m)]
    (is (mat/e= av (vec (mat/eseq m))))
    (is (mat/e= (mat/reshape av (mat/shape m)) m))))

(defn test-assign [m]
  (when (and m (pos? (mat/ecount m))) ;; guard for nil and empty arrays
    (let [e (first (mat/eseq m))
          n (mat/assign m e)
          mm (mat/mutable m)]
      (is (mat/zero-dimensional? e))
      (is (mat/e= (mat/broadcast e (mat/shape m)) n))
      (is (mat/same-shape? m n))
      (mat/fill! mm e)
      (is (mat/e= mm n)))))

(defn check-joined-matrices [dim original joined]
  (is (== (mat/dimensionality original) (mat/dimensionality joined)))
  (let [js (mat/slices joined dim)
        os (mat/slices original dim)
        cnt (mat/dimension-count original dim)]
    (is (== (mat/dimension-count joined dim) (* 2 cnt)))
    (when (pos? cnt)
      (is (mat/e= os (take cnt js)))
      (is (mat/e= os (drop cnt js))))))

(defn test-join
  "Test for joining matrices along major dimension"
  ([m]
   (when (and m (== 1 (mat/dimensionality m)))
     (let [j (mat/join m m)]
       (check-joined-matrices 0 m j)))))


(defn test-join-along
  "Test for joining matrices along arbitrary dimensions"
  ([m]
    (test-join-along m 0)
    (test-join-along m 1)
    (test-join-along m 2))
  ([m dim]
    (when (< dim (mat/dimensionality m))
      (let [j (mat/join-along dim m m)]
        (check-joined-matrices dim m j)))))

; TODO: Figure out pretty-print support for Clojurescript
#?(:clj
(defn test-pm
  "Test for matrix pretty-printing"
  ([m]
    (is (pos? (count (with-out-str (mat/pm m)))))))
)

(defn test-to-string [m]
  (when m ;; guard for nil
    (is (string? (str m)))))

(defn test-elements [m]
  (let [es (mat/eseq m)]
    (testing "scalar should be equivalent to identity function on elements"
      (is (= (vec es) (map mat/scalar es))))))

(defn test-array-output [m]
  (let [arr (mat/to-object-array m)]
    (testing "object array should equal element sequence"
      (is (= (seq (mat/eseq m)) (seq arr))))))

(defn test-mutable-select-view [m]
  (let [dims (mat/dimensionality m)]
    (testing "select should match select-view"
      (when (> (mat/ecount m) 0)
        (let [area (repeat dims 0)]
         (is (mat/e= (apply mat/select m area) (apply mat/select-view m area))))))
    (when (== 2 dims)
      (testing "select-view on a matrix should return a mutable column"
        (let [m (mat/mutable m)
              sm (mat/select-view m :all 0)]
          (mat/assign! sm 7)
          (is (mat/equals sm (mat/get-column m 0))))))))

(defn test-array-assumptions [m]
  ;; note: these must work on *any* array, i.e. no pre-assumptions on element type etc.
  (test-as-vector m)
  (test-coerce m)
  (test-pack m)
  (test-assign m)
  (test-join m)
  (test-join-along m)
  (test-dimensionality-assumptions m)
  (test-slice-assumptions m)
  (test-slice-returns-scalar-on-1d m)
  (test-submatrix-assumptions m)
  (test-mutable-assumptions m)
  (test-mutable-select-view m)
  (test-immutable-assumptions m)
  (test-vector-round-trip m)
  (test-ndarray-round-trip m)
  (test-reshape m)
  (test-rotate m)
  #?(:clj (test-pm m))
  (test-to-string m)
  (test-elements m)
  (test-array-output m)
  (test-broadcast m)
  (test-general-transpose m))

(defn test-assumptions-for-all-sizes [im]
  (doseq [vm (create-supported-matrices im)]
    (let [m (mat/matrix im vm)]
      (test-array-assumptions m)
      (test-double-array-ops m))))



;; ==============================================
;; misc tests

(defn test-implementation-namespace
  [im]
  :TODO)

;; TODO: figure out what to do with implementations that only support specific types?
(defn test-new-matrices [im]
  (testing "Vector construction"
    (when (mat/supports-dimensionality? im 1)
      (let [v (mat/matrix im [1])]
        (is (== 1.0 (mat/mget v 0))))
      (let [a (mat/new-array im [1])]
        (is (= (mat/mget a 0) (generic/default-value im))))))
  (testing "Matrix construction"
    (when (mat/supports-dimensionality? im 2)
      (let [m (mat/matrix im [[1 2] [3 4]])]
        (is (== 3.0 (mat/mget m 1 0))))))
  (testing "All supported sizes")
    (doseq [vm (create-supported-matrices im)]
      (let [m (mat/matrix im vm)]
        (is (mat/e== vm m)))))

(defn test-coerce-via-vectors [m]
  (testing "Vector coercion"
    (when (mat/supports-dimensionality? m 1)
      ; Probably can't assume this? may coerce to a different implementation
      ; (testing "coerce works"
      ;   (is (= (imp/get-implementation-key m) (imp/get-implementation-key (mat/coerce m [1])))))
      (let [v (mat/matrix [1])]
        (is (mat/equals [1] (mat/to-nested-vectors v))))))
  (testing "Matrix coercion"
    (when (mat/supports-dimensionality? m 2)
;      TODO: figure out if this is valid or not?
;      (testing "coerce works"
;        (is (= (imp/get-implementation-key m) (imp/get-implementation-key (mat/coerce m [[1 2] [3 4]])))))
      (let [m (mat/matrix [[1 2] [3 4]])]
        (is (mat/equals [[1 2] [3 4]] (mat/to-nested-vectors m))))))
;  (testing "Invalid vectors"
;    (is (error? (matrix m [1 [2 3]])))
;    (is (error? (matrix m [[2 3] 1]))))
;
)

(defn test-dimensionality [im]
  (testing "supported matrix size tests"
    (doseq [vm (create-supported-matrices im)]
      (let [m (mat/coerce im vm)]
        (is (= (mat/shape m) (mat/shape vm)))
        (is (= (mat/ecount m) (mat/ecount vm)))
        (is (mat/e= m (mat/emap identity m)))))))

(defn test-equality [m]
  (testing "proper work of equality check"
    (is (mat/equals (mat/coerce m (mat/array [1]))
                (mat/coerce m (mat/array [1]))))
    (is (not (mat/equals (mat/coerce m (mat/array [1]))
                     (mat/coerce m (mat/array 1)))))))

#?(:clj (do

(defn method-exists? [method im args]
  (try
    (apply method im (rest args))
    true
    (catch AbstractMethodError e false)
    (catch Throwable e true)))

(defn test-methods-existence [m]
  (let [im-name (mp/implementation-key m)]
    (if (#{:nd-wrapper
           :slice-wrapper
           :scalar-wrapper
           :dataset} im-name)
      true
      (doseq [proto (u/extract-protocols)]
        (doseq [[_ {:keys [name arglists]}] (:sigs proto)
                :let [method (ns-resolve 'clojure.core.matrix.protocols
                                         name)]]
          (is (method-exists? method m (first arglists))
              (str "check method " name
                   " of implementation " im-name)))))))
))

;; =======================================
;; array interop tests

(defn test-array-assignment
  [im]
  (when (mat/mutable? im)
    (doseq [vm (create-supported-matrices im)]
      (let [m (mat/coerce im vm)
            len (mat/ecount m)
            vs (range 1 (inc len))
            arr (into-array vs)]
        (is (= len (count vs)))
        (is (every? true? (map == vs (mat/eseq m))))
        (when-not (mat/mutable? m)
          (error "Problem: coerced object not mutable?"))
        (mat/scale! m 0.0)
        (is (== 0.0 (first (mat/eseq m))))
        (mat/assign-array! m arr)
        (is (every? true? (map == vs (mat/eseq m))))))))

(defn test-array-interop [im]
  (test-array-assignment im))

;; ========================================
;; numeric function tests

(defn test-scale
  ([m]
   (is (mat/equals [] (mat/mul! (mat/new-array m [0]) 0.95)))
   (is (mutable-equivalent? m #(mat/scale! % 2) #(mat/scale % 2)))
   (is (mutable-equivalent? m #(mat/mul! % 2) #(mat/mul % 2)))))

(defn test-numeric-functions [im]
  (when (mat/supports-dimensionality? im 2)
    (let [m (mat/matrix im [[1 2] [3 4]])]
      (is (== 10 (mat/esum m)))
      (test-scale m)))
  (when (mat/supports-dimensionality? im 1)
    (let [m (mat/matrix im [1 2 3])]
      (is (== 6 (mat/esum m)))
      (test-scale m))))

;; ========================================
;; arbitrary numeric instance tests

;(defn test-generic-numerical-assumptions [m]
;  (is (mat/equals m (add m (generic/zero m))))
;  (is (mat/equals m (mul m (generic/one m)))))

(defn numeric-scalar-tests [m]
  (is (mat/equals (mat/scalar-array 0) (mat/new-scalar-array m)))
  (is (mat/equals m (mat/add m (mat/new-scalar-array m))))
  (is (mat/equals m (mat/add (mat/scalar-array m 0) m))))

(defn misc-numeric-tests [m]
  (is (mat/equals m (mat/sparse m)))
  (is (mat/equals m (mat/dense m)))
  #?(:clj (is (mat/equals m (clojure.core.matrix.impl.double-array/to-double-arrays m))))
  (is (mat/equals (mat/add m m) (mat/scale m 2.0)))
  (is (mat/equals (mat/square m) (ops/** m 2) 0.0001))
  (is (mat/equals m (ops/** m 1)))
  (is (mat/equals (mat/sub m 0.0) (mat/scale m 1.0)))
  (is (mat/equals (mat/negate m) (mat/outer-product -1.0 m)))
  (is (mat/equals (mat/add 0.0 m) (mat/mul 1 m)))
  (is (mat/equals m (mat/div m 1)))
  (let [sh (mat/shape m)
        doubles (mat/to-double-array m)]
    (is (== (count doubles) (mat/ecount m)))
    (is (mat/equals m (mat/reshape doubles sh) 0.0001)))
  (let [m (mat/add (mat/square m) 1)]
    (is (mat/equals m (mat/div (mat/square m) m) 0.0001)))
  (is (mat/equals (mat/mul m m) (mat/square m)))
  (is (mat/equals (mat/esum m) (mat/ereduce + m) 0.0001))
  (is (= (seq (map inc (mat/eseq m))) (seq (mat/eseq (mat/emap inc m)))))
  (if (#{:vectorz} (mat/current-implementation))
    (let [v (->> #(rand 1000.0) repeatedly (take 5) vec mat/normalise mat/array)
          i (mat/identity-matrix 5)
          m (mat/sub i (mat/mul 2.0 (mat/outer-product v v)))]
      (is (mat/equals m (mat/transpose m) 1.0E-12))
      (is (mat/equals m (mat/inverse m) 1.0E-12))
      (is (mat/equals (mat/mmul m m) i 1.0E-12))))
  (let [m1 (mat/matrix m [[1 2 3 4]])
        m2 (mat/matrix m [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
        m3 (mat/matrix m [[2 3]
                      [5 6]
                      [7 8]])]
    (is (mat/equals (mat/shape (mat/mmul m1 m2)) [1 3]))
    (is (mat/equals (mat/shape (mat/mmul m2 m3)) [4 2]))))

(defn test-numeric-matrix-predicates [m]
  (when (== 2 (mat/dimensionality m))
    (is (mat/zero-matrix? (mat/new-matrix m 10 10)))
    (is (mat/zero-matrix? (mat/zero-matrix m 10 10)))
    (is (mat/identity-matrix? (mat/identity-matrix m 5)))
    (is (not (mat/identity-matrix? (mat/array m [[2 0][0 1]]))))
    (is (not (mat/zero-matrix? (mat/array m [[0 0][0 1]]))))
    (is (mat/identity-matrix? (mat/array m [[1.0 0.0][0.0 1.0]])))
    (is (mat/zero-matrix? (mat/array m [[0.0]])))
    (is (not (mat/identity-matrix? (mat/array m [[1.0 0.0]]))))))

(defn test-numeric-matrix-types [m]
  (when (== 2 (mat/dimensionality m))
    (let [m1 (mat/diagonal-matrix m [1 2 3])
          m2 (mat/matrix m [[1 2 3] [0 4 5] [0 0 6]])
          m3 (mat/matrix m [[1 0 0] [2 3 0] [3 4 5]])
          m4 (mat/matrix m [[0 -0.8 -0.6] [0.8 -0.36 0.48] [0.6 0.48 -0.64]])]
      (is (mat/diagonal? m1))
      (is (mat/upper-triangular? m1))
      (is (mat/lower-triangular? m1))
      (is (mat/upper-triangular? m2))
      (is (not (mat/lower-triangular? m2)))
      (is (not (mat/diagonal? m2)))
      (is (mat/lower-triangular? m3))
      (is (not (mat/upper-triangular? m3)))
      (is (not (mat/diagonal? m3)))
      (is (mat/orthogonal? m4))
      (is (not (mat/orthogonal? m1)))
      (is (not (mat/orthogonal? m2))))))

(defn test-numeric-instance [m]
  (is (mat/numerical? m))
 ; (test-generic-numerical-assumptions m)
  (numeric-scalar-tests m)
  (misc-numeric-tests m)
  (test-numeric-matrix-predicates m)
  (test-numeric-matrix-types m))

;; ========================================
;; 1D vector tests

(defn test-vector-slices [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (= [3] (seq (mat/shape m))))
    (is (mat/equals m (mat/matrix im (mat/coerce [] (mat/slices m)))))
    (is (= (mapv mp/get-0d (mat/slices m)) (vec (mat/eseq m))))))

(defn test-vector-subvector [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (mat/equals [2] (mat/subvector m 1 1)))
    (is (mat/equals m (mat/subvector m 0 3)))))

(defn test-element-add [im]
  (is (mat/equals [2.0 4.0] (mat/emap + (mat/matrix im [1 3]) (mat/coerce im [1 1]))))
  (is (mat/equals [2.0 4.0] (mat/emap inc (mat/matrix im [1 3])))))

(defn test-vector-mset [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (mat/equals [1 2 4] (mat/mset m 2 4)))
    (is (mat/equals [1 2 3] m))))

(defn test-vector-cross [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (mat/equals [0 0 0] (mat/cross m m)))
    (is (mat/equals [-1 2 -1] (mat/cross m [1 1 1])))
    (is (mutable-equivalent? m #(mat/cross! % [3 4 5]) #(mat/cross % [3 4 5])))))

(defn test-vector-products [m]
  (let [a (mat/matrix m [1 2 3])
        b (mat/matrix m [4 5 6])]
    (is (mat/equals [[4 5 6] [8 10 12] [12 15 18]] (mat/outer-product a b)))
    (is (mat/equals 32 (mat/inner-product a b)))))

(defn test-vector-mutable-add [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (mutable-equivalent? m #(mat/add! % [3 4 5]) #(mat/add % [3 4 5])))
    (is (mutable-equivalent? m #(mat/sub! % [3 4 5]) #(mat/sub % [3 4 5])))))

(defn test-vector-length [im]
  (let [m (mat/matrix im [3 4])]
    (is (== 5 (mat/length m)))
    (is (== 25 (mat/dot m m)))))

(defn test-vector-normalise [im]
  ;; we need to check if implementation supports non-integer values
  (when (-> (mat/matrix im [0 0]) (mat/assign 2.5) (mat/equals [2.5 2.5]))
    (let [m (mat/matrix im [3 4])
          n (mat/normalise m)]
      (is (mat/equals n [0.6 0.8] 0.000001))
      (is (mutable-equivalent? m mat/normalise! mat/normalise)))))

(defn test-vector-distance [im]
  (let [a (mat/matrix im [1 1])
        b (mat/matrix im [4 -3])]
    (is (== 5 (mat/distance a b)))))

(defn test-1d-instances [im]
  (test-numeric-instance (mat/matrix im [-1 2 -3]))
  (test-numeric-instance (mat/matrix im [1]))
  (test-numeric-instance (mat/matrix im [1 2 -3 4.5 7 -10.8]))
  (test-numeric-instance (mat/matrix im [0 0])))

(defn test-1d-mmul [im]
  (let [m (mat/matrix im [1 2 3])]
    (is (mat/equals 14 (mat/mmul m m)))))

(defn vector-tests-1d [im]
  (test-vector-mset im)
  (test-vector-length im)
  (test-vector-cross im)
  (test-vector-mutable-add im)
  (test-vector-normalise im)
  (test-vector-slices im)
  (test-vector-subvector im)
  (test-vector-distance im)
  (test-element-add im)
  (test-1d-instances im)
  (test-1d-mmul im)
  (test-vector-products im))

;; ========================================
;; 2D matrix tests

(defn test-transpose [im]
  (testing "2D transpose"
    (let [m (mat/matrix im [[1 2] [3 4]])]
      (is (mat/equals [[1 3] [2 4]] (mat/transpose m)))
      (is (mat/equals m (mat/transpose (mat/transpose m)))))))

(defn test-order [im]
  (testing "order"
    (let [m (mat/matrix im [[1 2 4] [4 5 6]])]
      (is (mat/equals [[1 2] [4 5]] (mat/order m 1 [0 1]))))))

(defn test-negate [im]
  (testing "negate"
    (let [m (mat/matrix im [[1 2] [3 4]])]
      (is (mat/equals [[-1 -2] [-3 -4]] (mat/negate m))))))

(defn test-identity [im]
  (let [I (mat/identity-matrix im 3)
        test-mtx [[1 2 3] [4 5 6] [7 8 9]]]
    (is (mat/equals [1 2 3] (mat/mmul I [1 2 3])))
    (is (mat/equals test-mtx (mat/mmul I test-mtx)))
    (is (mat/equals I (mat/transpose I)))))

(defn test-trace [im]
  (let [I (mat/identity-matrix im 3)]
    (is (== 3.0 (mat/trace I))))
  (let [m (mat/matrix im [[1 2] [3 4]])]
    (is (== 5.0 (mat/trace m)))))

(defn test-diagonal [im]
  (let [I (mat/diagonal-matrix im [1 2 3])
        I-squared (mat/diagonal-matrix im [1 4 9])]
    (is (mat/equals [1 4 9] (mat/mmul I [1 2 3])))
    (is (mat/equals I-squared (mat/mmul I I)))
    (is (mat/equals I (mat/transpose I))))
  (let [m1 (mat/matrix im [[1 2 3] [4 5 6] [7 8 9]])
        m2 (mat/matrix im [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
        m3 (mat/matrix im [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
    (is (mat/equals [1 5 9] (mat/main-diagonal m1)))
    (is (mat/equals [1 5 9] (mat/main-diagonal m2)))
    (is (mat/equals [1 6 11] (mat/main-diagonal m3)))))

(defn test-row-column-matrices [im]
  (let [rm (mat/row-matrix im [1 2 3])]
    (is (= [1 3] (seq (mat/shape rm))))
    (is (mat/equals [[1 2 3]] rm))
    (is (mat/row-matrix? rm))
    (is (mat/column-matrix? (mat/transpose rm))))
  (let [cm (mat/column-matrix im [1 2 3])]
    (is (= [3 1] (seq (mat/shape cm))))
    (is (mat/equals [[1] [2] [3]] cm))
    (is (mat/column-matrix? cm))
    (is (mat/row-matrix? (mat/transpose cm)))))

(defn test-matrix-mul [im]
  (is (mat/equals [[2 2] [4 4]] (mat/e* (mat/matrix im [[1 1] [2 2]]) 2)))
  (is (mat/equals [[2 2] [4 4]] (mat/e* 2 (mat/matrix im [[1 1] [2 2]]))))
  (when (mat/supports-dimensionality? im 1)
    (is (mat/equals [[1 2] [1 2]] (mat/broadcast (mat/matrix im [1 2]) [2 2])))))

(defn test-matrix-mset [im]
  (let [m (mat/matrix im [[1 2] [3 4]])]
    (is (mat/equals [[5 2] [3 4]] (mat/mset m 0 0 5)))
    (is (mat/equals [[1 2] [5 4]] (mat/mset m 1 0 5)))
    (is (mat/equals [[1 2] [3 5]] (mat/mset m 1 1 5)))))

(defn test-matrix-selection [im]
  (let [m (mat/matrix im [[1 2] [3 4]])]
    (is (mat/equals [1 2] (mat/select m 0 :all)))
    (if (mat/supports-dimensionality? m 1)
      (is (mat/equals [1 4] (mat/select-indices m [[0 0] [1 1]]))))))

(defn test-matrix-set-selection [im]
  (let [m (mat/matrix im [[1 2] [3 4]])
        mutable-m (mat/ensure-mutable m)]
    (is (mat/equals [[1 1] [1 1]] (mat/set-selection m :all :all 1)))
    (is (mat/equals [[5 2] [6 4]] (mat/set-selection m :all 0 [5 6])))))

(defn test-2d-instances [im]
  (test-numeric-instance (mat/matrix im [[1 2] [3 4]]))
  (test-numeric-instance (mat/matrix im [[1 2]]))
  (test-numeric-instance (mat/matrix im [[10]]))
  (test-numeric-instance (mat/matrix im [[10] [11]])))

(defn test-matrix-slices [im]
  (let [m (mat/matrix im [[1 2 3] [4 5 6]])]
    (is (mat/equals [1 2 3] (mat/get-row m 0)))
    (is (mat/equals [2 5] (mat/get-column m 1)))
    (is (mat/equals [4 5 6] (mat/slice m 1)))
    (is (mat/equals [3 6] (mat/slice m 1 2)))))

(defn test-matrix-set-column
  [im]
  (let [m (mat/matrix im [[1 2] [3 4]])
        mutable-m (mat/ensure-mutable m)]
    (is (mat/equals [[1 5] [3 5]] (mat/set-column m 1 5)))
    (is (mat/equals [[1 5] [3 6]] (mat/set-column m 1 [5 6])))
    (mat/set-column! mutable-m 0 7)
    (is (mat/equals [[7 2] [7 4]] mutable-m))))


(defn matrix-tests-2d [im]
  (test-row-column-matrices im)
  (test-transpose im)
  (test-diagonal im)
  (test-trace im)
  (test-matrix-mul im)
  (test-identity im)
  (test-order im)
  (test-2d-instances im)
  (test-matrix-mset im)
  (test-matrix-slices im)
  (test-matrix-set-column im)
  (test-matrix-selection im)
  (test-matrix-set-selection im))

;; ======================================
;; Instance test function
;;
;; Implementations can call to test specific instances of interest
;;
;; All matrix implementations must pass this test for any valid matrix
(defn instance-test [m]
  (try
    (when (mat/numerical? m)
      (test-numeric-instance m))
    (test-array-assumptions m)
    #?(:clj (catch #?(:clj Throwable :cljs js/Error) t
              (throw (RuntimeException. (str "Error testing instance: " m) t)))
       :cljs (catch js/Error t
              (throw (js/Error. (str "Error testing instance: " m) t))))))

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
  (doseq [dim (range 10)] (is (mat/supports-dimensionality? im dim)))
  (doseq [m (create-supported-matrices im)] (instance-test m))
  (instance-test (mat/coerce im ['a 'b]))
  (instance-test (mat/coerce im [[[[[["foo"]]]]]])))

;; =====================================
;; Row Operations Tests

 (defn test-row-operations
   [im]
     (is (mat/e== [0 2] (mat/swap-rows (mat/matrix im [2 0]) 0 1)))
     (is (mat/e== [2 2 3] (mat/multiply-row (mat/matrix im [1 2 3]) 0 2)))
     (is (mat/e== [3 1] (mat/add-row (mat/matrix im [1 1]) 0 1 2))))

;; ======================================
;; Decompositions Tests

(defn test-qr
  [im]
  (dorun
   (map
    #(let [m (mat/matrix im %)
           {:keys [Q R]} (linear/qr m)]
       (is (mat/equals m (mat/mmul Q R) 0.000001)))
    [[[1 2 3 4]
      [0 0 10 0]
      [3 0 5 6]]
     [[1 1 1
       0 1 1
       0 0 1]]
     [[1 7 3]
      [7 4 -5]
      [3 -5 6]]]))

  (let [m (mat/matrix im [[1 2] [3 4] [5 6] [7 8]])]
    (let [{:keys [R]} (linear/qr m {:return [:R]
                             :compact true})]
      (is (= (mat/row-count R) 2)))
    (let [{:keys [Q R]} (linear/qr m {:compact true})]
      (is (mat/equals (mat/shape Q) [4 4]))
      (is (mat/equals (mat/shape R) [2 2])))))

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
  (let [im (or (imp/get-canonical-object m)
               (error "Implementation not registered: " (#?(:clj class :cljs type) m)))
        im (mat/clone im) ;; clone to avoid risk of modifying canonical object
        ik (imp/get-implementation-key im)]
    (binding [imp/*matrix-implementation* ik]
      (instance-test im)
      (test-implementation im)
      (test-assumptions-for-all-sizes im)
      (test-coerce-via-vectors im)
      (test-equality im)
      #?(:clj (test-methods-existence im))
      (when (mat/supports-dimensionality? im 2)
        (matrix-tests-2d im))
      (when (mat/supports-dimensionality? im 1)
        (vector-tests-1d im))
      (test-array-interop im)
      (test-numeric-functions im)
      (test-dimensionality im)
      (test-row-operations im)
      (test-qr im)
      (test-new-matrices im))))

