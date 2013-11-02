(ns clojure.core.matrix.impl.default
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.double-array])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.implementations :as imp]))

;; =========================================================================
;; This namespace contains default implementations for core.matrix protocols
;; 
;; These will be used for any protocol that is not extended to an array type
;;
;; In general, default implementations are provided for:
;; - nil : treated as a nil scalar value
;; - java.lang.Number : treated as a numerical scalar value
;; - java.lang.Object : any unrecognised object, will be treated as an array
;;

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ============================================================
;; Utility functions for default implementations

(defn array?
  "Returns true if the parameter is an N-dimensional array of any type"
  ([m]
    (satisfies? mp/PImplementation m)))

(defn square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn- calc-element-count
  "Returns the total count of elements in an array"
  ([m]
    (cond
      (array? m) (reduce * 1 (mp/get-shape m))
      :else (count m))))

;; TODO: make smarter for different numeric types
(defn construct-mutable-matrix
  "Constructs a new mutable matrix with the given data."
  ([m]
    (let [dims (long (mp/dimensionality m))
          type (mp/element-type m)
          double? (or (= Double/TYPE type))]
      (cond
        (== dims 0)
          (wrap/wrap-scalar (mp/get-0d m))
        (and (== dims 1) double?)
          (clojure.core.matrix.impl.double-array/construct-double-array m)
        double?
          (mp/coerce-param (imp/get-canonical-object :ndarray-double) m)
        :else
          (mp/coerce-param (imp/get-canonical-object :ndarray) m)))))

;; ============================================================
;; Default implementations
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise (java.lang.Object) we should try to
;;   implement in terms of simpler operations, on assumption that
;;   we have fallen through to the default implementation

;; default implementation for matrix ops

(extend-protocol mp/PIndexedAccess
  nil
    (get-1d [m x]
      (error "Can't do 1D get on nil"))
    (get-2d [m x y]
      (error "Can't do 2D get on nil"))
    (get-nd [m indexes]
      (if-let [s (seq indexes)]
        (error "Can't do ND get on nil with indexes: " s)
        m))
  Number
    (get-1d [m x]
      (error "Can't do 1D get on a scalar number"))
    (get-2d [m x y]
      (error "Can't do 2D get on a scalar number"))
    (get-nd [m indexes]
      (if-let [s (seq indexes)]
        (error "Can't do ND get on a scalar number with indexes: " s)
        m))
  Object
    (get-1d [m x] (mp/get-nd m [x]))
    (get-2d [m x y] (mp/get-nd m [x y]))
    (get-nd [m indexes]
      (if (seq indexes)
        (error "Indexed get failed, not defined for:" (class m))
        (mp/get-0d m))))

(extend-protocol mp/PArrayMetrics
  nil
    (nonzero-count [m] 1)
  Number
    (nonzero-count [m] (if (zero? m) 0 1))
  Object
    (nonzero-count [m] 
      (mp/element-reduce m (fn [cnt e] (if (zero? e) cnt (inc cnt))) 0)))

(extend-protocol mp/PZeroDimensionConstruction
  nil
    (new-scalar-array
      ([m] 0.0)
      ([m value]
        (wrap/wrap-scalar value)))
  Object
    (new-scalar-array
      ([m] (wrap/wrap-scalar 0.0))
      ([m value] (wrap/wrap-scalar value))))

(extend-protocol mp/PZeroDimensionAccess
  nil
    (get-0d [m]
      nil)
    (set-0d! [m value]
      (error "Can't set the value of nil!"))
  Number
    (get-0d [m]
      m)
    (set-0d! [m value]
      (error "Can't set a scalar number!"))
  Object
    (get-0d [m]
      (if (mp/is-scalar? m) m (mp/get-nd m [])))
    (set-0d! [m value]
      (mp/set-nd! m [] value)))

(extend-protocol mp/PZeroDimensionSet
  nil
    (set-0d [m value]
      value ;; should be OK, since scalars satisfy 0d array abstraction
      )
  Object
    (set-0d [m value]
      value ;; should be OK, since scalars satisfy 0d array abstraction
      )) 

(extend-protocol mp/PIndexedSetting
  nil 
    (set-1d [m row v]
      (error "Can't do 1D set on nil"))
    (set-2d [m row column v]
      (error "Can't do 2D set on nil"))
    (set-nd [m indexes v]
      (if (seq indexes) 
        (error "Can't do " (count indexes) "D set on nil")
        v))
    (is-mutable? [m]
      false)
  Number 
    (set-1d [m row v]
      (error "Can't do 1D set on a scalar number"))
    (set-2d [m row column v]
      (error "Can't do 2D set on a scalar number"))
    (set-nd [m indexes v]
      (if (seq indexes) 
        (error "Can't do " (count indexes) "D set on a scalar number")
        v))
    (is-mutable? [m]
      false) 
  Object
    (set-1d [m row v]
      (let [m (mp/clone m)]
        (mp/set-1d! m row v)
        m))
    (set-2d [m row column v]
      (let [m (mp/clone m)]
        (mp/set-2d! m row column v)
        m))
    (set-nd [m indexes v]
      (let [m (mp/clone m)]
        (mp/set-nd! m indexes v)
        m))
    (is-mutable? [m]
      true))

(extend-protocol mp/PNumerical
  Number
    (numerical? [m]
      true)
  nil
    (numerical? [m]
      false)
  Object
    (numerical? [m]
      (if (mp/is-scalar? m)
        false ;; it's a scalar but not a number, so must not be numerical
              ;; TODO: probably needs special handling for generic numerical types?
        (every? number? (mp/element-seq m)))))

(extend-protocol mp/PVectorOps
  Number
    (vector-dot [a b] (* a b))
    (length [a] (double a))
    (length-squared [a] (Math/sqrt (double a)))
    (normalise [a]
      (let [a (double a)]
        (cond
          (> a 0.0) 1.0
          (< a 0.0) -1.0
          :else 0.0)))
  Object
    (vector-dot [a b]
      (mp/element-sum (mp/element-multiply a b)))
    (length [a]
      (Math/sqrt (double (mp/length-squared a))))
    (length-squared [a]
      (mp/element-reduce a (fn [r x] (+ r (* x x))) 0))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PVectorDistance
  Number
    (distance [a b] (Math/abs (double (- b a))))
  Object
    (distance [a b] (double (mp/length (mp/matrix-sub a b)))))

(extend-protocol mp/PVectorCross
  Object
    (cross-product [a b]
      (let [x1 (double (mp/get-1d a 0))
            y1 (double (mp/get-1d a 1))
            z1 (double (mp/get-1d a 2))
            x2 (double (mp/get-1d b 0))
            y2 (double (mp/get-1d b 1))
            z2 (double (mp/get-1d b 2))]
        (mp/construct-matrix a [(- (* y1 z2) (* z1 y2))
                                (- (* z1 x2) (* x1 z2))
                                (- (* x1 y2) (* y1 x2))])))
    (cross-product! [a b]
      (let [x1 (double (mp/get-1d a 0))
            y1 (double (mp/get-1d a 1))
            z1 (double (mp/get-1d a 2))
            x2 (double (mp/get-1d b 0))
            y2 (double (mp/get-1d b 1))
            z2 (double (mp/get-1d b 2))]
        (mp/set-1d! a 0 (- (* y1 z2) (* z1 y2)))
        (mp/set-1d! a 1 (- (* z1 x2) (* x1 z2)))
        (mp/set-1d! a 2 (- (* x1 y2) (* y1 x2)))
        a)))

(extend-protocol mp/PMutableVectorOps
  Object
    (normalise! [a]
      (mp/scale! a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PAssignment
  Object
    (assign! [m x]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== 1 dims)
              (let [xdims (long (mp/dimensionality x))
                    msize (long (mp/dimension-count m 0))]
                (if (== 0 xdims)
                  (let [value (mp/get-0d x)]
                    (dotimes [i msize] (mp/set-1d! m i value)))
                  (dotimes [i msize] (mp/set-1d! m i (mp/get-1d x i)))))
          (== 0 dims) (mp/set-0d! m (mp/get-0d x))
            (array? m)
            (let [xdims (long (mp/dimensionality x))]
              (if (> xdims 0)
                (doall (map (fn [a b] (mp/assign! a b)) (mp/get-major-slice-seq m) (mp/get-major-slice-seq x)))
                (let [value (mp/get-0d x)]
                  (doseq [ms (mp/get-major-slice-seq m)] (mp/assign! ms value)))))
            :else
              (error "Can't assign to a non-array object: " (class m)))))
    (assign-array!
      ([m arr]
          (let [alen (long (count arr))]
            (if (mp/is-vector? m)
              (dotimes [i alen]
                (mp/set-1d! m i (nth arr i)))
              (mp/assign-array! m arr 0 alen))))
      ([m arr start length]
          (let [length (long length)
                start (long start)]
         (if (mp/is-vector? m)
              (dotimes [i length]
                (mp/set-1d! m i (nth arr (+ start i))))
              (let [ss (seq (mp/get-major-slice-seq m))
                    skip (long (if ss (calc-element-count (first (mp/get-major-slice-seq m))) 0))]
                (doseq-indexed [s ss i]
                  (mp/assign-array! s arr (+ start (* skip i)) skip))))))))

(extend-protocol mp/PImmutableAssignment
  nil
    (assign [m source]
      (let [r (mp/broadcast-coerce m source)]
        (if (identical? r source) (mp/clone r) r)))
  Object
    (assign [m source]
      (let [r (mp/broadcast-coerce m source)]
        (if (identical? r source) (mp/clone r) r))))

(extend-protocol mp/PMutableFill
  Object
    (fill! [m value]
      (mp/assign! m value)))

(extend-protocol mp/PMatrixCloning
   nil
     (clone [m]
       m)
   Number
     (clone [m]
       m)
   Object
     (clone [m]
       (mp/construct-matrix m m)))

(extend-protocol mp/PSparseArray
   Object
     (is-sparse? [m]
       false))

(extend-protocol mp/PZeroCount
  nil 
    (zero-count [m]
      0)
  Number
     (zero-count [m]
       (if (zero? m) 1 0))
  Object
     (zero-count [m]
       (mp/element-reduce m (fn [acc e] (if (zero? e) (inc acc) acc)) 0)))


(extend-protocol mp/PMutableMatrixConstruction
  nil
    (mutable-matrix [m]
      (wrap/wrap-scalar m))
  Number
    (mutable-matrix [m]
      (wrap/wrap-scalar m))
  Object
    (mutable-matrix [m]
      (construct-mutable-matrix m)))

(extend-protocol mp/PComputeMatrix
  Object
    (compute-matrix [m shape f]
      (let [m (mp/new-matrix-nd m shape)]
        (reduce (fn [m ix] (mp/set-nd m ix (apply f ix))) m (base-index-seq-for-shape shape)))))

(extend-protocol mp/PDimensionInfo
  nil
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "cannot get dimension count from nil"))
  Number
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "Number has zero dimensionality, cannot get dimension count"))
  Object
    (dimensionality [m] 0)
    (is-vector? [m] false)
    (is-scalar? [m] true) ;; assume objects are scalars unless told otherwise
    (get-shape [m] nil)
    (dimension-count [m i] (error "Can't determine count of dimension " i " on Object: " (class m))))

(extend-protocol mp/PSameShape
  nil
    (same-shape? [a b]
      (== 0 (mp/dimensionality b)))
  Number 
    (same-shape? [a b]
      (== 0 (mp/dimensionality b)))
  Object
    (same-shape? [a b]
      (same-shape-object? (mp/get-shape a) (mp/get-shape b))))

;; generic versions of matrix ops
(extend-protocol mp/PMatrixOps
  nil
    (trace [m] m)
  Number
    (trace [m] m)
  Object
    (trace [m]
      (when-not (== 2 (mp/dimensionality m)) (error "Trace requires a 2D matrix"))
      (let [rc (mp/dimension-count m 0)
            cc (mp/dimension-count m 1)
            dims (long rc)]
        (when-not (== rc cc) (error "Can't compute trace of non-square matrix"))
        (loop [i 0 res 0.0]
          (if (>= i dims)
            res
            (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (determinant [m]
      (->> m
           (mp/coerce-param (imp/get-canonical-object :ndarray-double))
           (mp/determinant)))
    (inverse [m]
      (->> m
           (mp/coerce-param (imp/get-canonical-object :ndarray-double))
           (mp/inverse)
           (mp/coerce-param m))))

(extend-protocol mp/PTranspose
  nil
    (transpose [m] m)
  Number
    (transpose [m] m)
  Object
    (transpose [m]
      (case (long (mp/dimensionality m))
        0 m
        1 m
        2 (mp/coerce-param m (apply mapv vector (map
                                                  #(mp/coerce-param [] %)
                                                  (mp/get-major-slice-seq m))))
        (mp/coerce-param m
          (let [ss (map mp/transpose (mp/get-major-slice-seq m))]
            ;; note that function must come second for mp/element-map
            (case (count ss)
              1 (mp/element-map (mp/coerce-param [] (first ss)) vector)
              2 (mp/element-map (mp/coerce-param [] (first ss)) vector (second ss))
              (mp/element-map (mp/coerce-param [] (first ss)) vector (second ss) (nnext ss))))))))

(extend-protocol mp/PTransposeInPlace
  Object
    (transpose! [m]
      (let [n (long (mp/dimension-count m 0))]
        (dotimes [i n]
          (dotimes [j i]
            (let [t (mp/get-2d m i j)]
              (mp/set-2d! m i j (mp/get-2d m j i))
              (mp/set-2d! m j i t)))))))

(extend-protocol mp/PMatrixProducts
  Number
    (inner-product [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
    (outer-product [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
  Object
    (inner-product [m a]
      (cond
        (mp/is-scalar? m)
          (mp/pre-scale a m)
        (mp/is-scalar? a)
          (mp/scale m a)
        (== 1 (mp/dimensionality m))
          (reduce mp/matrix-add (map (fn [sl x] (mp/scale sl x))
                                     (mp/get-major-slice-seq a)
                                     (mp/get-major-slice-seq m))) ;; TODO: implement with mutable accumulation
        :else
          (mp/coerce-param m
            (mapv #(mp/inner-product % a) (mp/get-major-slice-seq m)))))
    (outer-product [m a]
      (cond
        (mp/is-scalar? m)
          (mp/pre-scale a m)
        :else
          (mp/coerce-param m (mp/convert-to-nested-vectors
                               (mp/element-map m (fn [v] (mp/pre-scale a v))))))))

;; matrix multiply
;; TODO: document returning NDArray
(extend-protocol mp/PMatrixMultiply
  Number
    (element-multiply [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
    (matrix-multiply [m a]
      (cond
        (number? a) (* m a)
        (array? a) (mp/pre-scale a m)
        :else (error "Don't know how to multiply number with: " (class a))))
  Object
    (matrix-multiply [m a]
      (let [mdims (long (mp/dimensionality m))
            adims (long (mp/dimensionality a))]
        (cond
         (== adims 0) (mp/scale m a)
         (and (== mdims 1) (== adims 2))
           (let [[arows acols] (mp/get-shape a)]
             (mp/reshape (mp/matrix-multiply (mp/reshape m [1 arows]) a)
                         [arows]))
         (and (== mdims 2) (== adims 1))
           (let [[mrows mcols] (mp/get-shape m)]
             (mp/reshape (mp/matrix-multiply m (mp/reshape a [mcols 1]))
                         [mcols]))
         (and (== mdims 2) (== adims 2))
           (let [mutable (mp/is-mutable? m)
                 [mrows mcols] (mp/get-shape m)
                 [arows acols] (mp/get-shape a)
                 new-m-type (if mutable m (imp/get-canonical-object :ndarray))
                 new-m (mp/new-matrix new-m-type mrows acols)]
             (do
               ;; TODO: optimize cache-locality (http://bit.ly/12FgFbl)
               (c-for [i (long 0) (< i mrows) (inc i)
                       j (long 0) (< j acols) (inc j)]
                 (mp/set-2d! new-m i j 0))
               (c-for [i (long 0) (< i mrows) (inc i)
                       j (long 0) (< j acols) (inc j)
                       k (long 0) (< k mcols) (inc k)]
                 (mp/set-2d! new-m i j (+ (mp/get-2d new-m i j)
                                          (* (mp/get-2d m i k)
                                             (mp/get-2d a k j)))))
               new-m)))))
    (element-multiply [m a]
      (if (number? a)
        (mp/scale m a)
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m clojure.core/* a)))))

;; matrix multiply
(extend-protocol mp/PMatrixMultiplyMutable
  Number
    (element-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
    (matrix-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
  Object
    (element-multiply! [m a]
      (mp/element-map! m * (mp/broadcast-like m a)))
    (matrix-multiply! [m a]
      (mp/assign! m (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixDivide
  Number
    (element-divide
      ([m] (/ m))
      ([m a] (mp/element-map a #(/ m %))))
  Object
    (element-divide
      ([m] (mp/element-map m #(/ %)))
      ([m a]
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m #(/ %1 %2) a)))))

;; matrix element summation
(extend-protocol mp/PSummable
  Number
    (element-sum [a] a)
  Object
    (element-sum [a]
      (mp/element-reduce a +)))

;; add-product operations
(extend-protocol mp/PAddProduct
  Number
    (add-product [m a b]
      (+ m (* a b)))
  Object
    (add-product [m a b]
      (mp/matrix-add m (mp/element-multiply a b))))

(extend-protocol mp/PAddProductMutable
  Number
    (add-product! [m a b]
      (error "Numbers are not mutable"))
  Object
    (add-product! [m a b]
      (mp/matrix-add! m (mp/element-multiply a b))))

(extend-protocol mp/PAddScaled
  Number
    (add-scaled [m a factor]
      (+ m (* a factor)))
  Object
    (add-scaled [m a factor]
      (mp/matrix-add m (mp/scale a factor))))

(extend-protocol mp/PAddScaledMutable
  Number
    (add-scaled! [m a factor]
      (error "Numbers are not mutable"))
  Object
    (add-scaled! [m a factor]
      (mp/matrix-add! m (mp/scale a factor))))

(extend-protocol mp/PAddScaledProduct
  Number
    (add-scaled-product [m a b factor]
      (+ m (* a b factor)))
  Object
    (add-scaled-product [m a b factor]
      (mp/matrix-add m (mp/scale (mp/element-multiply a b) factor))))

(extend-protocol mp/PAddScaledProductMutable
  Number
    (add-scaled-product! [m a b factor]
      (error "Numbers are not mutable"))
  Object
    (add-scaled-product! [m a b factor]
      (mp/matrix-add! m (mp/scale (mp/element-multiply a b) factor))))

;; type of matrix element
;; the default is to assume any type is possible
(extend-protocol mp/PTypeInfo
  nil
    (element-type [a]
      java.lang.Object)
  Object
    (element-type [a]
      java.lang.Object))

;; general transformation of a vector
(extend-protocol mp/PVectorTransform
  clojure.lang.IFn
    (vector-transform [m a]
      (m a))
    (vector-transform! [m a]
      (mp/assign! a (m a)))
  Object
    (vector-transform [m a]
      (cond
        (== 2 (mp/dimensionality m)) (mp/matrix-multiply m a)
        :else (error "Don't know how to transform using: " (class m))))
    (vector-transform! [m a]
      (mp/assign! a (mp/vector-transform m a))))

;; matrix scaling
(extend-protocol mp/PMatrixScaling
  Number
    (scale [m a]
      (if (number? a)
        (* m a)
        (mp/pre-scale a m)))
    (pre-scale [m a]
      (if (number? a)
        (* a m)
        (mp/scale a m)))
  Object
    (scale [m a]
      (mp/element-map m #(* % a)))
    (pre-scale [m a]
      (mp/element-map m (partial * a))))

(extend-protocol mp/PMatrixMutableScaling
  Number
    (scale! [m a]
      (error "Can't scale! a numeric value: " m))
    (pre-scale! [m a]
      (error "Can't pre-scale! a numeric value: " m))
  Object
    (scale! [m a]
      (mp/element-map! m #(* % a))
      m)
    (pre-scale! [m a]
      (mp/element-map! m (partial * a))
      m))

(extend-protocol mp/PMatrixAdd
  ;; matrix add for scalars
  Number
    (matrix-add [m a]
      (if (number? a) (+ m a)
        (mp/matrix-add a m)))
    (matrix-sub [m a]
      (if (number? a) (- m a)
        (mp/negate (mp/matrix-sub a m))))
  ;; default impelementation - assume we can use emap?
  Object
    (matrix-add [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m clojure.core/+ a)))
    (matrix-sub [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m clojure.core/- a))))

(extend-protocol mp/PMatrixAddMutable
  ;; matrix add for scalars
  Number
    (matrix-add! [m a]
      (error "Can't do mutable add! on a scalar number"))
    (matrix-sub! [m a]
      (error "Can't do mutable sub! on a scalar number"))
  ;; default impelementation - assume we can use emap?
  Object
    (matrix-add! [m a]
      (mp/element-map! m clojure.core/+ a))
    (matrix-sub! [m a]
      (mp/element-map! m clojure.core/- a)))

(extend-protocol mp/PNegation
  nil
    (negate [m]
      (error "Can't negate nil!"))
  Number
    (negate [m]
      (- m))
  Object
    (negate [m]
      (mp/scale m -1)))

;; equality checking
(extend-protocol mp/PMatrixEquality
  nil
    (matrix-equals [a b]
      (error "nil is not a valid numerical value in equality testing"))
  Number
    (matrix-equals [a b]
      (cond
        (number? b) (== a b)
        (== 0 (mp/dimensionality b)) (== a (mp/get-0d b))
        :else false))
  Object
    (matrix-equals [a b]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (if (== 0 (mp/dimensionality a))
            (== (mp/get-0d a) (mp/get-0d b))
            (not (some false? (map == (mp/element-seq a) (mp/element-seq b)))))
        :else false)))

(extend-protocol mp/PValueEquality
  nil
    (value-equals [a b]
      (and 
        (== 0 (mp/dimensionality b))
        (nil? (mp/get-0d b))))
  Object
    (value-equals [a b]
      (and
        (mp/same-shape? a b)
        (every? true? (map = (mp/element-seq a) (mp/element-seq b))))))

(defmacro eps== [a b eps]
  `(<= (Math/abs (- (double ~a) (double ~b))) (double ~eps) ))

;; equality checking
(extend-protocol mp/PMatrixEqualityEpsilon
  nil
    (matrix-equals-epsilon [a b eps]
      (error "nil is not a valid numerical value in equality testing"))
  Number
    (matrix-equals-epsilon [a b eps]
      (cond
        (number? b) (eps== a b eps)
        (== 0 (mp/dimensionality b)) (eps== a (mp/get-0d b) eps)
        :else false))
  Object
    (matrix-equals-epsilon [a b eps]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (let [eps (double eps)]
            (every? #(<= (Math/abs (double %)) eps) (map - (mp/element-seq a) (mp/element-seq b))))
        :else false)))

(extend-protocol mp/PDoubleArrayOutput
  Number
    (to-double-array [m] (aset (double-array 1) 0 (double m)))
    (as-double-array [m] nil)
  Object
    (to-double-array [m]
      (double-array (mp/element-seq m)))
    (as-double-array [m] nil))

;; row operations
(extend-protocol mp/PRowOperations
  Object
    (swap-rows [m i j]
      (mp/swap-rows (mp/coerce-param [] m) i j))
    (multiply-row [m i k]
      (mp/multiply-row (mp/coerce-param [] m) i k))
    (add-row [m i j k]
      (mp/add-row (mp/coerce-param [] m) i j k)))

(extend-protocol mp/PRowSetting
  Object
    (set-row [m i row]
      (let [svec (vec (mp/get-major-slice-seq m))
            row (mp/broadcast-like (svec 0) row)]
        (mp/coerce-param m (assoc svec i row))))
    (set-row! [m i row]
      (let [sl (mp/get-major-slice-view m i)
            row (mp/broadcast-like sl row)]
        (mp/assign! sl row)
        m)))

;; functional operations
(extend-protocol mp/PFunctionalOperations
  Number
    (element-seq [m]
      (list m))
    (element-map
      ([m f]
        (f m))
      ([m f a]
        (f m (mp/get-0d a)))
      ([m f a more]
        (apply f m (mp/get-0d a) (map mp/get-0d more))))
    (element-map!
      ([m f]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a more]
        (error "java.lang.Number instance is not mutable!")))
    (element-reduce
      ([m f]
        m)
      ([m f init]
        (f init m)))
  Object
    (element-seq [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== 0 dims)
            (list (mp/get-0d m))
          (== 1 dims)
            (map #(mp/get-1d m %) (range (mp/dimension-count m 0)))
          (array? m)
            (mapcat mp/element-seq (mp/get-major-slice-seq m))
          :else (error "Don't know how to create element-seq from: " m))))
    (element-map
      ([m f]
        (let [s (map f (mp/element-seq m))]
          (mp/reshape (mp/coerce-param m s)
                      (mp/get-shape m))))
      ([m f a]
        (let [s (map f (mp/element-seq m) (mp/element-seq a))]
          (mp/reshape (mp/coerce-param m s)
                      (mp/get-shape m))))
      ([m f a more]
        (let [s (map f (mp/element-seq m) (mp/element-seq a))
              s (apply map f (list* (mp/element-seq m)
                                    (mp/element-seq a)
                                    (map mp/element-seq more)))]
          (mp/reshape (mp/coerce-param m s)
                      (mp/get-shape m)))))
    (element-map!
      ([m f]
        (mp/assign! m (mp/element-map m f)))
      ([m f a]
        (mp/assign! m (mp/element-map m f a)))
      ([m f a more]
        (mp/assign! m (mp/element-map m f a more))))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m))))
  nil
    (element-seq [m] '(nil))
    (element-map
      ([m f] (f nil))
      ([m f a] (f nil a))
      ([m f a more] (apply f nil a more)))
    (element-map!
      ([m f] (error "Can't do element-map! on nil"))
      ([m f a] (error "Can't do element-map! on nil"))
      ([m f a more] (error "Can't do element-map! on nil")))
    (element-reduce
      ([m f] nil)
      ([m f init] (f init nil))))

(extend-protocol mp/PElementCount
  nil (element-count [m] 1)
  Number (element-count [m] 1)
  Object 
    (element-count [m] (if (array? m) 
                         (calc-element-count m)
                         1)))

(extend-protocol mp/PMatrixSlices
  Object
    (get-row [m i]
      (mp/get-major-slice m i))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (clojure.core.matrix.impl.wrappers/wrap-slice m i))
    (get-slice [m dimension i]
      (mp/get-slice (mp/coerce-param [] m) dimension i)))

(extend-protocol mp/PSliceView
  Object
    ;; default implementation uses a lightweight wrapper object
    (get-major-slice-view [m i] (clojure.core.matrix.impl.wrappers/wrap-slice m i)))

(extend-protocol mp/PSliceSeq
  Object
    (get-major-slice-seq [m]
      (let [dims (mp/dimensionality m)]
        (cond
          (<= dims 0)
            (error "Can't get slices on [" dims "]-dimensional object: " m)
          :else (map #(mp/get-major-slice m %) (range (mp/dimension-count m 0)))))))

(extend-protocol mp/PSliceJoin
  nil
    (join [m a] 
      (error "Can't join an array to a nil value!"))
  Number
    (join [m a]
      (error "Can't join an array to a scalar number!"))
  Object
    (join [m a]
      (let [dims (mp/dimensionality m)
            adims (mp/dimensionality m)]
        (cond
          (== dims 0)
            (error "Can't join to a 0-dimensional array!")
          (== dims adims)
            (mp/coerce-param m (concat (mp/get-major-slice-seq m) (mp/get-major-slice-seq a)))
          (== dims (inc adims))
            (mp/coerce-param m (concat (mp/get-major-slice-seq m) [a]))
          :else
            (error "Joining with array of incompatible size")))))

(extend-protocol mp/PSubVector
  nil
    (subvector [m start length]
      (error "Can't take subvector of nil"))
  Number
    (subvector [m start length]
      (error "Can't take subvector of a scalar number"))
  Object
    (subvector [m start length]
      (mp/subvector (wrap/wrap-nd m) start length)))

(extend-protocol mp/PSubMatrix
  nil
    (submatrix [m index-ranges]
      (if (seq index-ranges)
        (error "Can't take partial submatrix of nil")
        m))
  Number
    (submatrix [m index-ranges]
      (if (seq index-ranges)
        (error "Can't take partial submatrix of a scalar number")
        m))
  Object
    (submatrix [m index-ranges]
      (clojure.core.matrix.impl.wrappers/wrap-submatrix m index-ranges)))

(extend-protocol mp/PBroadcast
  nil
    (broadcast [m new-shape]
      (clojure.core.matrix.impl.wrappers/wrap-broadcast m new-shape))
  Object
    (broadcast [m new-shape]
      (let [nshape new-shape
            mshape (mp/get-shape m)
            mdims (count mshape)
            ndims (count nshape)]
        (cond
          (and (== mdims ndims) (same-shape-object? nshape mshape)) m
          ;(and (> ndims mdims) (== mshape (drop (- ndims mdims) nshape)))
          ;  (let [rep (nth nshape (- ndims mdims 1))]
          ;    (mp/broadcast (vec (repeat rep m)) new-shape))
          :else (clojure.core.matrix.impl.wrappers/wrap-broadcast m new-shape)))))

(extend-protocol mp/PBroadcastLike
  nil
    (broadcast-like [m a]
      (clojure.core.matrix.impl.wrappers/wrap-broadcast a (mp/get-shape m)))
  Object
    (broadcast-like [m a]
      (let [sm (mp/get-shape m) sa (mp/get-shape a)]
        (if (clojure.core.matrix.utils/same-shape-object? sm sa)
          a
          (mp/broadcast a sm)))))

(extend-protocol mp/PBroadcastCoerce
  nil
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a)))
  Object
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a))))

;; attempt conversion to nested vectors
(extend-protocol mp/PConversion
  nil
    (convert-to-nested-vectors [m]
      nil)
  Number
    (convert-to-nested-vectors [m]
      ;; we accept a scalar as a "nested vector" for these purposes
      m)
  Object
    (convert-to-nested-vectors [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== dims 0)
              (mp/get-0d m)
          (== 1 dims)             
            (if (or (seq? m) (sequential? m))
              (mapv mp/get-0d m)
              (let [n (long (mp/dimension-count m 0))]
                (loop [i 0 res []]
                  (if (< i n)
                    (recur (inc i) (conj res (mp/get-1d m i)))
                    res))))
          (sequential? m)
              (mapv mp/convert-to-nested-vectors m)
          (seq? m)
              (mapv mp/convert-to-nested-vectors m)
          (array? m)
              (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))
          :default
              (error "Can't work out how to convert to nested vectors: " (class m) " = " m)))))

(extend-protocol mp/PVectorView
  nil
    (as-vector [m]
      [nil])
  Number
    (as-vector [m]
      [m])
  Object
    (as-vector [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== dims 0)
            (mp/broadcast (wrap/wrap-nd m) [1])
          (== dims 1)
            m
          (not (mp/is-mutable? m))
            ;; if not mutable, coercion to a vector works as a view
            (mp/to-vector m)
          :else
            ;; We return nil for this: can't provide a mutable vector view
            nil))))

(extend-protocol mp/PVectorisable
  nil
    (to-vector [m]
      [nil])
  Number
    (to-vector [m]
      [m])
  Object
    (to-vector [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== 0 dims)
            (mp/coerce-param m [(mp/get-0d m)])
          (mp/is-vector? m)
            (mp/clone m)
          :else
            (mp/coerce-param m (mp/element-seq m))))))

(extend-protocol mp/PReshaping
  nil
    (reshape [m shape]
      (case (long (reduce * 1 (seq shape)))
        0 (mp/broadcast m shape)
        1 (mp/broadcast m shape)
        (error "Can't reshape nil to shape: " (vec shape))))
  Number
    (reshape [m shape]
      (case (long (reduce * 1 (seq shape)))
        0 (mp/broadcast m shape)
        1 (mp/broadcast m shape)
        (error "Can't reshape a scalar number to shape: " (vec shape))))
  Object
    (reshape [m shape]
      (let [partition-shape (fn partition-shape [es shape]
                              (if-let [s (seq shape)]
                                (let [ns (next s)
                                      plen (reduce * 1 ns)]
                                  (map #(partition-shape % ns) (partition plen es)))
                                (first es)))]
        (if-let [shape (seq shape)]
          (let [fs (long (first shape))
                parts (partition-shape (mp/element-seq m) shape)]
            (when-not (<= fs (count parts))
              (error "Reshape not possible: insufficient elements for shape: " shape " have: " (seq parts)))
            (mp/construct-matrix m (take fs parts)))
          (first (mp/element-seq m))))))

(extend-protocol mp/PCoercion
  nil
    (coerce-param [m param]
      param)
  Number
    (coerce-param [m param]
      param)
  Object
    (coerce-param [m param]
      (mp/construct-matrix m (mp/convert-to-nested-vectors param))))

(extend-protocol mp/PExponent
  Number
    (element-pow [m exponent]
      (Math/pow (.doubleValue m) (double exponent)))
  Object
    (element-pow [m exponent]
      (let [x (double exponent)]
        (mp/element-map m #(Math/pow (.doubleValue ^Number %) x)))))

(extend-protocol mp/PSquare
  Number
   (square [m] (* m m))
  Object
   (square [m] (mp/element-multiply m m)))

;; define standard Java maths functions for numbers
(eval
  `(extend-protocol mp/PMathsFunctions
     Number
       ~@(map (fn [[name func]]
                `(~name [~'m] (double (~func (double ~'m)))))
              mops/maths-ops)
     Object
       ~@(map (fn [[name func]]
                `(~name [~'m] (mp/element-map ~'m #(double (~func (double %))))))
              mops/maths-ops)))

(eval
  `(extend-protocol mp/PMathsFunctionsMutable
     Number
       ~@(map (fn [[name func]]
                `(~(symbol (str name "!")) [~'m] (error "Number is not mutable!")))
              mops/maths-ops)
     Object
       ~@(map (fn [[name func]]
                `(~(symbol (str name "!")) [~'m] (mp/element-map! ~'m #(double (~func (double %))))))
              mops/maths-ops)))

(extend-protocol mp/PMatrixSubComponents
  Object
    (main-diagonal [m]
      (let [sh (mp/get-shape m)
            rank (count sh)
            dims (apply min sh)]
        (mp/construct-matrix m (for [i (range dims)] (mp/get-nd m (repeat rank i)))))))

(extend-protocol mp/PSpecialisedConstructors
  Object
    (identity-matrix [m dims]
      (mp/diagonal-matrix m (repeat dims 1.0)))
    (diagonal-matrix [m diagonal-values]
      (let [dims (count diagonal-values)
            diagonal-values (mp/coerce-param [] diagonal-values)
            zs (vec (repeat dims 0.0))
            dm (vec (for [i (range dims)]
                 (assoc zs i (nth diagonal-values i))))]
        (mp/coerce-param m dm))))

(extend-protocol mp/PPermutationMatrix
  Object
    (permutation-matrix [m permutation]
      (let [v (mp/convert-to-nested-vectors permutation)
            n (count v)
            r (mp/new-matrix m n n)
            r (if (mp/is-mutable? r) r (construct-mutable-matrix r))]
        (dotimes [i n]
          (mp/set-2d! r i (v i) 1.0))
        r)))

(extend-protocol mp/PMatrixPredicates
  Object
  (identity-matrix? [m]
    (let [rc (long (mp/dimension-count m 0))
          cc (long (mp/dimension-count m 1))]
      (if (and (== (mp/dimensionality m) 2) (== rc cc))
        (loop [i (long 0)]
          (if (< i rc)
            (if (loop [j (long 0)]
                  (if (< j cc)
                    (let [elem (mp/get-2d m i j)]
                      (if (nil? elem)
                        false
                        (if (== i j)
                          (if (== elem 1) (recur (inc j)) false)
                          (if (== elem 0) (recur (inc j)) false))))
                    true))
              (recur (inc i))
              false)
            true))
        false)))
  (zero-matrix? [m]
    (every? #(and (number? %) (zero? %)) (mp/element-seq m)))
  nil
  (identity-matrix? [m] false)
  (zero-matrix? [m] false))


;; =======================================================
;; default multimethod implementations

(defmethod mm/mul :default [x y]
  (mp/inner-product x y))
