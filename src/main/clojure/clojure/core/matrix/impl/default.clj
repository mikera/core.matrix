(ns clojure.core.matrix.impl.default
  (:require [clojure.core.matrix.impl.double-array :as da]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :refer :all])
  (:import [clojure.lang ISeq]))

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

(defmacro array?
  "Returns true if the parameter is an N-dimensional array of any type"
  ([m]
    `(not (mp/is-scalar? ~m))))

(defn- square?
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
          (da/construct-double-array m)
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

;; default overall implementation

(extend-protocol mp/PImplementation
  Object
    (implementation-key [m] :default)
    (meta-info [m] {})
    (construct-matrix [m data]
      (mp/construct-matrix [] data))
    (new-vector [m length]
      (mp/new-vector [] length))
    (new-matrix [m rows columns]
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m shape]
      (mp/new-matrix-nd [] shape))
    (supports-dimensionality? [m dimensions]
      true)
  
  ;; keyword implementation looks up implementation by keyword
  clojure.lang.Keyword
    (implementation-key [m] m)
    (meta-info [m] (mp/meta-info (imp/get-canonical-object-or-throw m)))
    (construct-matrix [m data]
      (mp/construct-matrix (imp/get-canonical-object-or-throw m) data))
    (new-vector [m length]
      (mp/new-vector (imp/get-canonical-object-or-throw m) length))
    (new-matrix [m rows columns]
      (mp/new-matrix (imp/get-canonical-object-or-throw m) rows columns))
    (new-matrix-nd [m shape]
      (mp/new-matrix-nd (imp/get-canonical-object-or-throw m) shape))
    (supports-dimensionality? [m dimensions]
      (mp/supports-dimensionality? (imp/get-canonical-object-or-throw m) dimensions)))

(extend-protocol mp/PSparse
  nil
    (sparse-coerce [m data]
      (mp/sparse data))
    (sparse [m]
      nil)
  Object
    (sparse-coerce [m data]
      nil) ;; allow fall through if sparse coercion is not directly supported
    (sparse [m]
      m))

(extend-protocol mp/PNewSparseArray
  Object
    (new-sparse-array [m shape]
      ;; we don't support sparse arrays by default, so just return nil
      nil))

(extend-protocol mp/PDense
  nil
    (dense-coerce [m data]
      (mp/dense data))
    (dense [m]
      nil)
  Object
    (dense-coerce [m data]
      nil) ;; allow fall-through if dense coercion is not directly supported
    (dense [m]
      m))

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
    (get-1d [m x]
      (cond
        (java-array? m) (mp/get-0d (nth m x))
        :else (mp/get-nd m [x])))
    (get-2d [m x y]
      (cond
        (java-array? m) (mp/get-1d (nth m x) y)
        :else (mp/get-nd m [x y])))
    (get-nd [m indexes]
      (if (seq indexes)
        (cond
          (java-array? m) (mp/get-nd (nth m (first indexes)) (next indexes))
          :else (error "Indexed get failed, not defined for:" (class m)))
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
      ;; assume an object is mutable unless we know otherwise. 
      ;; certainly true for arbitrary Java arrays, for example.
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
        false ;; it's a scalar but not a number, so we do not recognise it as numerical
        (every? number? (mp/element-seq m)))))

(extend-protocol mp/PVectorOps
  Number
    (vector-dot [a b] (mp/pre-scale b a))
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
    (distance [a b] 
      (if (number? b) 
        (Math/abs (double (- b a)))
        (mp/distance b a)))
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
          (== 0 dims) (mp/set-0d! m (mp/get-0d x))
          (== 1 dims)
            (if (instance? ISeq x)
              (let [x (seq x)
                    msize (long (mp/dimension-count m 0))]
                (loop [i 0 s (seq x)]
                  (if (>= i msize)
                    (when s (error "Mismatches size of sequence in assign!"))
                    (do
                      (mp/set-1d! m i (first s))
                      (recur (inc i) (next s))))))
             (let [xdims (long (mp/dimensionality x))
                    msize (long (mp/dimension-count m 0))]
                (if (== 0 xdims)
                  (let [value (mp/get-0d x)]
                    (dotimes [i msize] (mp/set-1d! m i value)))
                  (dotimes [i msize] (mp/set-1d! m i (mp/get-1d x i))))))


          (array? m)
            (let [xdims (long (mp/dimensionality x))]
              (if (pos? xdims)
                (let [xss (mp/get-major-slice-seq x)
                      _ (or (mp/same-shapes? xss) (error "Inconsistent slice shapes for assign!"))]
                  (doall (map (fn [a b] (mp/assign! a b)) (mp/get-major-slice-seq m) xss)))
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

(extend-protocol mp/PImmutableMatrixConstruction
  nil
    (immutable-matrix [m]
      nil)
  Object
    (immutable-matrix [m]
      (if (mp/is-mutable? m)
        (mp/convert-to-nested-vectors m)
        m)))

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
    (dimension-count [m i] (error "nil has zero dimensionality, cannot get count for dimension: " i))
  Number
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "Number has zero dimensionality, cannot get count for dimension: " i))
  Object
    (dimensionality [m]
      (cond
        (.isArray (.getClass m))
          (let [n (count m)]
            (if (> n 0) (inc (mp/dimensionality (nth m 0))) 1))
        :else 0))
    (is-vector? [m]
      (cond
        (.isArray (.getClass m))
          (let [n (count m)]
            (or (== n 0) (== 0 (mp/dimensionality (nth m 0)))))
        :else false))
    (is-scalar? [m]
      (cond
        (.isArray (.getClass m)) false
        :else true)) ;; assume objects are scalars unless told otherwise
    (get-shape [m]
      (cond
        (.isArray (.getClass m))
          (let [n (count m)]
            (if (== n 0) [0] (cons n (mp/get-shape (nth m 0)))))
        :else nil))
    (dimension-count [m i]
      (cond
        (.isArray (.getClass m))
          (if (== i 0) (count m) (mp/dimension-count (nth m 0) (dec i)))
        (== 0 i)
          (count m)
        :else (error "Can't determine count of dimension " i " on Object: " (class m)))))

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
      (let [rc (long (mp/dimension-count m 0))
            cc (long (mp/dimension-count m 1))
            dims (Math/min rc cc)]
        (loop [i 0 res 0.0]
          (if (>= i dims)
            res
            (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (determinant [m]
      (let [imp (or (imp/get-canonical-object :vectorz) (error "(let Need to load an implementation which supports determinant, e.g. vectorz-clj"))
            m (mp/coerce-param imp m)]
        (mp/determinant m)))
    (inverse [m]
      (let [imp (or (imp/get-canonical-object :vectorz) (error "Need to load an implementation which supports inverse, e.g. vectorz-clj"))
            mm (mp/coerce-param imp m)]
        (mp/coerce-param m (mp/inverse mm)))))

(extend-protocol mp/PTranspose
  nil
    (transpose [m] m)
  Number
    (transpose [m] m)
  Object
    (transpose [m]
      (mp/coerce-param
       m
       (case (long (mp/dimensionality m))
         0 m
         1 m
         2 (apply mapv vector (map
                               #(mp/convert-to-nested-vectors %)
                               (mp/get-major-slice-seq m)))
         (let [ss (map mp/transpose (mp/get-major-slice-seq m))]
           ;; note that function must come second for mp/element-map
           (case (count ss)
             1 (mp/element-map (mp/convert-to-nested-vectors (first ss)) vector)
             2 (mp/element-map (mp/convert-to-nested-vectors (first ss)) vector (second ss))
             (mp/element-map (mp/convert-to-nested-vectors (first ss)) vector (second ss) (nnext ss))))))))

(extend-protocol mp/PTransposeInPlace
  Object
    (transpose! [m]
      (let [n (long (mp/dimension-count m 0))]
        (when (not= n (long (mp/dimension-count m 1))) (error "transpose! requires a quare matrix"))
        (dotimes [i n]
          (dotimes [j i]
            (let [t (mp/get-2d m i j)]
              (mp/set-2d! m i j (mp/get-2d m j i))
              (mp/set-2d! m j i t)))))
      m))

(extend-protocol mp/PRotate
  nil
    (rotate [m dim places] nil)
  Number
    (rotate [m dim places] m)
  Object
    (rotate [m dim places]
      (cond
        (<= (mp/dimensionality m) 0)
          m
        (== 0 dim)
          (let [ss (mp/get-major-slice-seq m)
                c (long (mp/dimension-count m 0))
                sh (long (if (pos? c) (long (mod places c)) 0))]
            (if (== sh 0)
              m
              (vec (concat (take-last (- c sh) ss) (take sh ss)))))
        :else
          (mp/rotate (mp/convert-to-nested-vectors m) dim places))))


(extend-protocol mp/PRotateAll
  nil
    (rotate-all [m shifts] nil)
  Number
    (rotate-all [m shifts] m)
  Object
    (rotate-all [m shifts]
      (reduce (fn [m [dim shift]] (if (zero? shift) m (mp/rotate m dim shift)))
         m
         (map-indexed (fn [i v] [i v]) shifts))))

(extend-protocol mp/PShift
  Object
    (shift [m dim shift] 
      (let [z (mp/generic-zero m)
            c (long (mp/dimension-count m dim))
            sh (vec (mp/get-shape m))]
        (cond 
          (== shift 0) m
          (>= shift c) (mp/broadcast-coerce m z)
          (<= shift (- c)) (mp/broadcast-coerce m z)
          (< shift 0) (mp/join-along 
                        (mp/broadcast (mp/construct-matrix m z) (assoc sh dim (- shift)))
                        (mp/submatrix m (map vector 
                                             (vec (repeat (count sh) 0)) 
                                             (assoc sh dim (+ c shift))))
                        dim)
          (> shift 0) (mp/join-along 
                        (mp/submatrix m (map vector 
                                             (assoc (vec (repeat (count sh) 0)) dim shift) 
                                             (assoc sh dim (- c shift))))
                        (mp/broadcast (mp/construct-matrix m z) (assoc sh dim shift))
                        dim)
          :else (error "Shouldn't be possible!!"))))
    (shift-all [m shifts]
      (reduce (fn [m [dim shift]] (if (zero? shift) m (mp/shift m dim shift)))
         m
         (map-indexed (fn [i v] [i v]) shifts))))


(extend-protocol mp/POrder
  nil
    (order 
      ([m indices] (error "Can't reorder a scalar nil"))
      ([m dim indices] (error "Can't reorder a scalar nil")))
  Number
    (order 
      ([m indices] (error "Can't reorder a scalar number"))
      ([m dim indices] (error "Can't reorder a scalar number")))
  Object
    (order 
      ([m indices] 
        (let [mshape (vec (mp/get-shape m))
              subshape (assoc m 0 1)
              ss (map #(mp/broadcast (mp/get-major-slice m %) subshape) indices)]
          (reduce #(mp/join %1 %2) ss)))
      ([m dim indices]
        (mp/order (mp/convert-to-nested-vectors m) dim indices))))


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
          (if (== 1 (mp/dimensionality a))
            (mp/element-sum (mp/element-multiply m a))
            (reduce mp/matrix-add (map (fn [sl x] (mp/scale sl x))
                                       (mp/get-major-slice-seq a)
                                       (mp/get-major-slice-seq m)))) ;; TODO: implement with mutable accumulation
        :else
          (mapv #(mp/inner-product % a) (mp/get-major-slice-seq m))))
    (outer-product [m a]
      (cond
        (mp/is-scalar? m)
          (mp/pre-scale a m)
        :else
          (mp/convert-to-nested-vectors
            (mp/element-map m (fn [v] (mp/pre-scale a v)))))))

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
         (and (== mdims 1) (== adims 1)) (mp/vector-dot m a)
         (and (== mdims 1) (== adims 2))
           (let [[arows acols] (mp/get-shape a)]
             (mp/reshape (mp/matrix-multiply (mp/reshape m [1 arows]) a)
                         [acols]))
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
      (mp/assign! m (mp/element-multiply m a)))
    (matrix-multiply! [m a]
      (mp/assign! m (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixDivide
  Number
    (element-divide
      ([m] (/ m))
      ([m a] (mp/pre-scale (mp/element-divide a) m)))
  Object
    (element-divide
      ([m] 
        (if (mp/get-shape m) 
          (mp/element-map m mp/element-divide)
          (error "Don't know how to take reciprocal of " (type m))))
      ([m a]
        (mp/element-multiply m (mp/element-divide a)))))

(extend-protocol mp/PMatrixDivideMutable
  Number
	  (element-divide!
	    ([m] (error "Can't do mutable divide on a scalar number"))
	    ([m a] (error "Can't do mutable divide on a scalar numer")))
  Object
	  (element-divide!
	    ([m] (mp/element-map! m #(/ %)))
	    ([m a]
	       (let [[m a] (mp/broadcast-compatible m a)]
	         (mp/element-map! m #(/ %1 %2) a)))))

;; matrix element summation
(extend-protocol mp/PSummable
  Number
    (element-sum [a] a)
  Object
    (element-sum [a]
      (mp/element-reduce a (if (mp/numerical? a) + mp/matrix-add))))

(extend-protocol mp/PElementMinMax
  Number
    (element-min [m] m)
    (element-max [m] m)
  Object
    (element-min [m]
      (mp/element-reduce m
                       (fn [best v] (if (or (not best) (< v best)) v best))
                       nil))
    (element-max [m]
      (mp/element-reduce m
                       (fn [best v] (if (or (not best) (> v best)) v best))
                       nil)))

;; add-product operations
(extend-protocol mp/PAddProduct
  Number
    (add-product [m a b]
      (mp/matrix-add (mp/element-multiply a b) m ))
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
      (mp/matrix-add (mp/scale a factor) m))
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
      (mp/matrix-add (mp/scale (mp/element-multiply a b) factor) m))
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
      Object)
  Object
    (element-type [a]
      (if (java-array? a)
        (.getComponentType (class a))
        Object)))

;; generic element values
(extend-protocol mp/PGenericValues
  Object
    (generic-zero [m]
      0)
    (generic-one [m]
      1)
    (generic-value [m]
      0))

;; general transformation of a vector
(extend-protocol mp/PVectorTransform
  clojure.lang.IFn
    (vector-transform [m a]
      (if
        (vector? m) (mp/matrix-multiply m a)
        (m a)))
    (vector-transform! [m a]
      (if
        (vector? m) (mp/assign! a (mp/matrix-multiply m a))
        (mp/assign! a (m a))))
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
        (== 0 (mp/dimensionality b)) (== a (scalar-coerce b))
        :else false))
  Object
    (matrix-equals [a b]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (if (== 0 (mp/dimensionality a))
            (== (mp/get-0d a) (scalar-coerce b))
            (not-any? false? (map == (mp/element-seq a) (mp/element-seq b))))
        :else false)))

(extend-protocol mp/PValueEquality
  nil
    (value-equals [a b]
      (or
        (nil? b)
        (and
          (== 0 (mp/dimensionality b))
          (nil? (mp/get-0d b)))))
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
    (to-double-array [m]
      (let [arr (double-array 1)] (aset arr 0 (double m)) arr))
    (as-double-array [m] nil)
  Object
    (to-double-array [m]
      (double-array (mp/element-seq m)))
    (as-double-array [m] nil))

(extend-protocol mp/PObjectArrayOutput
  nil
    (to-object-array [m]
      (let [arr (object-array 1)] arr))
    (as-object-array [m] nil)
  Number
    (to-object-array [m]
      (let [arr (object-array 1)] (aset arr 0 m) arr))
    (as-object-array [m] nil)
  Object
    (to-object-array [m]
      (object-array (mp/element-seq m)))
    (as-object-array [m] nil))

;; row operations
(extend-protocol mp/PRowOperations
  Object
    (swap-rows [m i j]
      (mp/swap-rows (mp/convert-to-nested-vectors m) i j))
    (multiply-row [m i k]
      (mp/multiply-row (mp/convert-to-nested-vectors m) i k))
    (add-row [m i j k]
      (mp/add-row (mp/convert-to-nested-vectors m) i j k)))

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

(extend-protocol mp/PColumnSetting
  Object
  (set-column [m i column]
    (let [scol (mp/get-column m 0)
          column (mp/broadcast-like scol column)
          indices (range (mp/dimension-count column 0))
          new-m (reduce (fn [acc idx]
                          (mp/set-2d acc idx i (mp/get-1d column idx)))
                        m indices)]
      (mp/coerce-param m new-m)))
  (set-column! [m i column]
    (let [scol (mp/get-column m 0)
          column (mp/broadcast-like scol column)]
      (dotimes [j (mp/dimension-count column 0)]
        (mp/set-2d! m j i (mp/get-1d column j))))))

;; functional operations
(extend-protocol mp/PFunctionalOperations
  Number
    (element-seq [m]
      (list m))
    (element-map
      ([m f]
        (f m))
      ([m f a]
        (mp/element-map a #(f m %)))
      ([m f a more]
        (if-let [moremore (next more)]
          (mp/element-map a #(apply f m %1 %2 %&) (first more) moremore)
          (mp/element-map a #(f m %1 %2) (first more)))))
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
      (let [c (.getClass m)
            dims (long (mp/dimensionality m))]
        (cond
          (== 0 dims)
            (list (mp/get-0d m))
          (and (.isArray c) (.isPrimitive (.getComponentType c)))
            (seq m)
          (== 1 dims)
            (map #(mp/get-1d m %) (range (mp/dimension-count m 0)))
          (array? m)
            (mapcat mp/element-seq (mp/get-major-slice-seq m))
          :else (error "Don't know how to create element-seq from: " m))))
    (element-map
      ([m f]
        (if (== 0 (mp/dimensionality m))
          (f (mp/get-0d m)) ;; handle case of single element
          (let [s (mapv f (mp/element-seq m))]
            (mp/reshape (mp/coerce-param m s)
                        (mp/get-shape m)))))
      ([m f a]
        (if (== 0 (mp/dimensionality m))
          (let [v (mp/get-0d m)]
            (mp/element-map a #(f v %)))
          (let [[m a] (mp/broadcast-compatible m a)
                s (mapv f (mp/element-seq m) (mp/element-seq a))]
            (mp/reshape (mp/coerce-param m s) ;; TODO: faster construction method?
                        (mp/get-shape m)))))
      ([m f a more]
        (let [s (mapv f (mp/element-seq m) (mp/element-seq a))
              s (apply mapv f (list* (mp/element-seq m)
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

(defn- cart [colls]
  (if (empty? colls)
    [[]]
    (for [x    (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn- indices-seq [m]
  (cart (map range (mp/get-shape m))))

(extend-protocol mp/PMapIndexed
  Number
    (element-map-indexed
      ([m f]
        (f [] m))
      ([m f a]
        (mp/element-map a #(f [] m %)))
      ([m f a more]
        (if-let [moremore (next more)]
          (mp/element-map a #(apply f [] m %1 %2 %&) (first more) moremore)
          (mp/element-map a #(f [] m %1 %2) (first more)))))
    (element-map-indexed!
      ([m f]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a]
        (error "java.lang.Number instance is not mutable!"))
      ([m f a more]
        (error "java.lang.Number instance is not mutable!")))
  Object
    (element-map-indexed
      ([m f]
        (if (== 0 (mp/dimensionality m))
          (f [] (mp/get-0d m)) ;; handle case of single element
          (let [s (map f (indices-seq m) (mp/element-seq m))]
            (mp/reshape (mp/coerce-param m s)
                        (mp/get-shape m)))))
      ([m f a]
        (if (== 0 (mp/dimensionality m))
          (let [v (mp/get-0d m)]
            (mp/element-map-indexed a #(f %1 v %2)))
          (let [[m a] (mp/broadcast-compatible m a)
                s (map f (indices-seq m) (mp/element-seq m) (mp/element-seq a))]
            (mp/reshape (mp/coerce-param m s) ;; TODO: faster construction method?
                        (mp/get-shape m)))))
      ([m f a more]
        (let [s (map f (mp/element-seq m) (mp/element-seq a))
              s (apply map f (list* (indices-seq m)
                                    (mp/element-seq m)
                                    (mp/element-seq a)
                                    (map mp/element-seq more)))]
          (mp/reshape (mp/coerce-param m s)
                      (mp/get-shape m)))))
    (element-map-indexed!
      ([m f]
        (mp/assign! m (mp/element-map-indexed m f)))
      ([m f a]
        (mp/assign! m (mp/element-map-indexed m f a)))
      ([m f a more]
        (mp/assign! m (mp/element-map-indexed m f a more))))
  nil
    (element-map-indexed
      ([m f] (f [] nil))
      ([m f a] (f [] nil a))
      ([m f a more] (apply f [] nil a more)))
    (element-map-indexed!
      ([m f] (error "Can't do element-map-indexed! on nil"))
      ([m f a] (error "Can't do element-map-indexed! on nil"))
      ([m f a more] (error "Can't do element-map-indexed! on nil"))))

(extend-protocol mp/PElementCount
  nil (element-count [m] 1)
  Number (element-count [m] 1)
  Object
    (element-count [m] (if (array? m)
                         (calc-element-count m)
                         1)))

(extend-protocol mp/PValidateShape
  nil
    (validate-shape [m]
      nil)
  Object
    (validate-shape [m]
      (cond
        (== 0 (mp/dimensionality m))
          (if (mp/is-scalar? m) nil [])
        :else
          (let [ss (mp/get-major-slice-seq m)
                shapes (mapv mp/validate-shape ss)]
            (if (mp/same-shapes? ss)
              (vec (cons (mp/dimension-count m 0) (first shapes)))
              (error "Inconsistent shapes for sub arrays in " (class m)))))))


(extend-protocol mp/PMatrixSlices
  Object
    (get-row [m i]
      (if (java-array? m)
        (nth m i)
        (mp/get-major-slice m i)))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (cond
       (java-array? m) (nth m i)
       (== 1 (mp/dimensionality m)) (mp/get-1d m i)
        :else (clojure.core.matrix.impl.wrappers/wrap-slice m i)))
    (get-slice [m dimension i]
      (cond
        (neg? dimension) (error "Can't take slice on negative dimension: " dimension)
        (== 0 dimension) (mp/get-major-slice m i)
        :else (mp/get-slice (mp/convert-to-nested-vectors m) dimension i))))

(extend-protocol mp/PMatrixColumns
  Object
  (get-columns [m]
    (case (long (mp/dimensionality m))
      0 (error "Can't get columns of a 0-dimensional object")
      1 (error "Can't get columns of a 1-dimensional object")
      2 (mp/get-slice-seq m 1)
      (mapcat mp/get-columns (mp/get-major-slice-seq m)))))

(extend-protocol mp/PMatrixRows
  Object
  (get-rows [m]
    (case (long (mp/dimensionality m))
      0 (error "Can't get rows of a 0-dimensional object")
      1 (error "Can't get rows of a 1-dimensional object")
      2 (mp/get-major-slice-seq m)
      (mapcat mp/get-rows (mp/get-major-slice-seq m)))))

(extend-protocol mp/PSliceView
  Object
    ;; default implementation uses a lightweight wrapper object
    (get-major-slice-view [m i]
      (cond
        (java-array? m)
          (let [ss (nth m i)]
            (if (array? ss)
              ss
              (clojure.core.matrix.impl.wrappers/wrap-slice m i)))
        :else (clojure.core.matrix.impl.wrappers/wrap-slice m i))))

(extend-protocol mp/PSliceSeq
  Object
    (get-major-slice-seq [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (<= dims 0) (error "Can't get slices on [" dims "]-dimensional object")
          (.isArray (.getClass m)) (seq m)
          (== dims 1) (map #(mp/get-1d m %) (range (mp/dimension-count m 0)))
          :else (map #(mp/get-major-slice m %) (range (mp/dimension-count m 0)))))))

(extend-protocol mp/PSliceSeq2
  Object
    (get-slice-seq [m dimension]
      (cond
        (== dimension 0) (mp/get-major-slice-seq m)
        (< dimension 0) (error "Can't get slices of a negative dimension: " dimension)
        :else (map #(mp/get-slice m dimension %) (range (mp/dimension-count m dimension))))))

(extend-protocol mp/PSliceViewSeq
  Object
    (get-major-slice-view-seq [m]
      (let [n (mp/dimension-count m 0)]
        (for [i (range n)]
          (mp/get-major-slice-view m i)))))

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
            adims (mp/dimensionality a)]
        (cond
          (== dims 0)
            (error "Can't join to a 0-dimensional array!")
          (== dims adims)
            (mp/coerce-param m (concat (mp/get-major-slice-seq m) (mp/get-major-slice-seq a)))
          (== dims (inc adims))
            (mp/coerce-param m (concat (mp/get-major-slice-seq m) [a]))
          :else
            (error "Joining with array of incompatible size")))))

(extend-protocol mp/PSliceJoinAlong
  nil
  (join-along [m a dim]
    (error "Can't join an array to a nil value!"))
  Number
  (join-along [m a dim]
    (error "Can't join an array to a scalar number!"))
  Object
  (join-along [m a dim]
    (mp/coerce-param m
      (cond
         (== dim 0)
           (mp/join m a)
         :else
           (mapv #(mp/join-along %1 %2 (dec dim))
                 (mp/get-major-slice-seq m)
                 (mp/get-major-slice-seq a))))))

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
      (wrap/wrap-submatrix m index-ranges)))

(extend-protocol mp/PBroadcast
  nil
    (broadcast [m new-shape]
      (wrap/wrap-broadcast m new-shape))
; TODO: efficient way to use current implementation?
;  Number
;    (broadcast [m new-shape]
;      (if (seq new-shape)
;        (mp/broadcast ())
;        m))
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
          :else (wrap/wrap-broadcast m new-shape)))))

(extend-protocol mp/PBroadcastLike
  nil
    (broadcast-like [m a]
      (wrap/wrap-broadcast a (mp/get-shape m)))
  Object
    (broadcast-like [m a]
      (let [sm (mp/get-shape m) sa (mp/get-shape a)]
        (if (same-shape-object? sm sa)
          a
          (mp/broadcast a sm)))))

(extend-protocol mp/PBroadcastCoerce
  nil
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a)))
  Object
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a))))

(extend-protocol mp/PPack
  nil
    (pack [m]
      nil)
  Object
    (pack [m]
      m))

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
          (array? m)
              (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))
          (sequential? m)
              (mapv mp/convert-to-nested-vectors m)
          (seq? m)
              (mapv mp/convert-to-nested-vectors m)
          :default
              (error "Can't work out how to convert to nested vectors: " (class m) " = " m)))))

(extend-protocol mp/PRowColMatrix
  nil
    (column-matrix [m data] (error "Can't create a column matrix from nil"))
    (row-matrix [m data] (error "Can't create a column matrix from nil"))
  Object
    (column-matrix [m data]
      (if (== 1 (mp/dimensionality data))
        (mp/coerce-param m (mapv vector (mp/element-seq data)))
        (error "Can't create a column matrix: input must be 1D vector")))
    (row-matrix [m data]
      (if (== 1 (mp/dimensionality data))
        (mp/coerce-param m (vector data)) ;; i.e. just wrap in a 
        (error "Can't create a row matrix: input must be 1D vector"))))

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
            [(mp/get-0d m)]
          (mp/is-vector? m)
            (mp/clone m)
          :else
            (vec (mp/element-seq m))))))

(extend-protocol mp/PReshaping
  nil
    (reshape [m shape]
      (mp/reshape [nil] shape))
  Number
    (reshape [m shape]
      (mp/reshape [m] shape))
  Object
    (reshape [m shape]
      (let [gv (mp/generic-value m) ;; generic value for array padding. Typically nil or zero
            es (concat (mp/element-seq m) (repeat gv))
            partition-shape (fn partition-shape [es shape]
                              (if-let [s (seq shape)]
                                (let [ns (next s)
                                      plen (reduce * 1 ns)]
                                  (map #(partition-shape % ns) (partition plen es)))
                                (first es)))]
        (if-let [shape (seq shape)]
          (let [fs (long (first shape))
                parts (partition-shape es shape)]
            (mp/construct-matrix m (take fs parts)))
          (first es)))))

(extend-protocol mp/PCoercion
  nil
    (coerce-param [m param]
      param)
  Number
    (coerce-param [m param]
      param)
  Object
    (coerce-param [m param]
      ;; NOTE: leave param unchanged if coercion not possible (probably an invalid shape for implementation)
      (let [param (if (instance? ISeq param) (mp/convert-to-nested-vectors param) param)] ;; ISeqs can be slow, so convert to vectors
        (or (mp/construct-matrix m param)
           param))))

(extend-protocol mp/PExponent
  Number
    (element-pow [m exponent]
      (if (array? exponent)
        (mp/element-map exponent #(Math/pow (.doubleValue m) (.doubleValue ^Number %)))
        (Math/pow (.doubleValue m) (double exponent))))
  Object
    (element-pow [m exponent]
      (if (array? exponent)
        (mp/element-map m #(Math/pow (.doubleValue ^Number %1) (.doubleValue ^Number %2)) exponent)
        (mp/element-map m #(Math/pow (.doubleValue ^Number %) exponent)))))

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
            dims (apply min sh)
            diag-vals (for [i (range dims)] (mp/get-nd m (repeat rank i)))]
        (imp/construct m diag-vals))))

(extend-protocol mp/PSpecialisedConstructors
  Object
    (identity-matrix [m dims]
      (mp/diagonal-matrix m (repeat dims 1.0)))
    (diagonal-matrix [m diagonal-values]
      (let [dims (count diagonal-values)
            diagonal-values (mp/convert-to-nested-vectors diagonal-values)
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

;; TODO: can this implementation be improved?
(extend-protocol mp/PBlockDiagonalMatrix
  Object
    (block-diagonal-matrix [m blocks]
      (let [aux (fn aux [acc blocks]
                  (if (empty? blocks)
                      acc
                      (let [acc-dim (mp/dimension-count acc 0)
                            new-block (blocks 0)
                            new-block-dim (mp/dimension-count new-block 0)
                            new-dim (+ acc-dim new-block-dim)
                            dm (vec (for [i (range new-dim)]
                                         (if (< i acc-dim)
                                             (into [] (concat (acc i)
                                                              (mp/new-vector [] new-block-dim)))
                                             (into [] (concat (mp/new-vector [] acc-dim)
                                                              (new-block (- i acc-dim)))))))]
                            (aux dm (subvec blocks 1)))))]
        (aux [] blocks))))

;; Helper function for symmetric? predicate in PMatrixPredicates.
;; Note loop/recur instead of letfn/recur is 20-25% slower.
(defn- symmetric-matrix-entries?
  "Returns true iff square matrix m is symmetric."
  [m]
  (let [dim (first (mp/get-shape m))]
    (letfn [(f [i j]
              (cond
                (>= i dim) true                         ; all entries match: symmetric
                (>= j dim) (recur (+ 1 i) (+ 2 i))      ; all j's OK: restart with new i
                (= (mp/get-2d m i j)
                   (mp/get-2d m j i)) (recur i (inc j)) ; OK, so check next pair
                :else false))]                          ; not same, not symmetric
      (f 0 1))))

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
  (symmetric? [m]
    (case (long (mp/dimensionality m))
      0 true
      1 true
      2 (and (square? m) (symmetric-matrix-entries? m))
      (= m (mp/transpose m))))
  nil
  (identity-matrix? [m] false)
  (zero-matrix? [m] false)
  (symmetric? [m] true))

;; ======================================================
;; default implementation for higher-level array indexing

(extend-protocol mp/PIndicesAccess
  Object
  (get-indices [a indices]
    (mp/construct-matrix (if (array? a) a [])
                         (map #(mp/get-nd a %1) (map mp/element-seq indices)))))

(extend-protocol mp/PIndicesSetting
  Object
  (set-indices [a indices values]
    (let [indices (map mp/element-seq indices)
          values (mp/element-seq (mp/broadcast values [(count indices)]))]
      (loop [a a [id & idx] indices [v & vs] values]
        (if id (recur (mp/set-nd a id v) idx vs) a))))
  (set-indices! [a indices values]
    (let [indices (map mp/element-seq indices)
          values (mp/element-seq (mp/broadcast values [(count indices)]))]
      (loop [[id & idx] indices [v & vs] values]
        (when id
          (mp/set-nd! a id v) (recur idx vs))))))

(extend-protocol mp/PNonZeroIndices
  Object
  (non-zero-indices
    [m]
    (if (mp/is-vector? m)
      (vec (for [i (range (mp/dimension-count m 0))
                    :when (not (== 0 (mp/get-1d m i)))]
              i))
      (vec (for [i (range (mp/dimension-count m 0))]
              (mp/non-zero-indices (mp/get-major-slice m i)))))))

;; TODO: proper generic implementations
(extend-protocol mp/PMatrixTypes
  Object
  (diagonal? [m]
    (if (= (mp/dimensionality m) 2)
      (let [[mrows mcols] (mp/get-shape m)]
        (->> (mp/element-seq m)
             (map #(vector (quot %1 mcols) (rem %1 mcols) %2)
                  (range (* mrows mcols)))
             (every? (fn [[i j v]]
                       (cond
                        (= i j) true
                        (and (not= i j) (== v 0)) true
                        :else false)))))
      false))
  (upper-triangular? [m]
    (if (square? m)
      (->> (mp/get-slice-seq m 0)
           (map vector (range))
           (mapcat (fn [[idx xs]] (take idx xs)))
           (every? zero?))
      false))
  (lower-triangular? [m]
    (if (square? m)
      (->> (mp/get-slice-seq m 0)
           (map vector (range))
           (mapcat (fn [[idx xs]] (drop (inc idx) xs)))
           (every? zero?))
      false))
  (positive-definite? [m]
    (error "TODO: Not yet implemented")
    (mp/positive-definite? (mp/convert-to-nested-vectors m)))
  (positive-semidefinite? [m]
    (error "TODO: Not yet implemented"))
  (orthogonal? [m eps]
    (and (square? m)
         (mp/matrix-equals-epsilon
           (mp/matrix-multiply m (mp/transpose m))
           (mp/identity-matrix m (mp/dimension-count m 0))
           eps))))

(extend-protocol mp/PSelect
  Object
  (select [a area]
    (wrap/wrap-selection a area)))

(defn- area-indices [area]
  (reduce (fn [io in]
            (for [a in b io]
              (cons a b))) (map vector (last area)) (rest (reverse area))))

(defn- indices [vals]
  (area-indices (map range (mp/get-shape vals))))


(extend-protocol mp/PSetSelection
  Object
  (set-selection [a area vals]
    (let [shape (map count area)
        vals (mp/broadcast vals shape)]
    (cond
     (and (= (count shape) 2)
          (= (first shape) (mp/dimension-count a 0)))
     (loop [a a [i & is] (second area) [j & js] (range (second shape))]
       (if i (recur (mp/set-column a i (mp/get-column vals j)) is js) a))
     (and (= (count shape) 2)
          (= (second shape) (mp/dimension-count a 1)))
     (loop [a a [i & is] (first area) [j & js] (range (first shape))]
       (if i (recur (mp/set-row a i (mp/get-row vals j)) is js) a))
     :else
     (loop [a a [idl & idxl] (area-indices area) [idr & idxr] (indices vals)]
       (if idl
         (recur (mp/set-nd a idl (mp/get-nd vals idr)) idxl idxr)
         a))))))

(extend-protocol mp/PIndexImplementation
  Object
	  (index? [m]
      false) ;; we default to saying something isn't an index, unless it is explicitly supported
	  (index-to-longs [m]
      (long-array (mp/element-seq m)))
	  (index-to-ints [m]
      (int-array (mp/element-seq m)))
	  (index-from-longs [m xs]
      (long-array xs))
	  (index-from-ints [m xs]
      (int-array xs))
	  (index-coerce [m a]
      (mp/index-to-longs m)))

(extend-protocol mp/PDimensionImplementation
  Object
    (dimension-name [ds idx dim]
      (cond
        (== dim 0) (mp/row-name ds idx)
        (== dim 1) (mp/column-name ds idx)
        :else idx))
    (row-name [ds idx]
      idx)
    (column-name [ds idx]
      (nth (mp/column-names ds) idx)))

;; =======================================================
;; default label implementation

(extend-protocol mp/PDimensionLabels
  Object
    (label [m dim i]
      (if (<= 0 (long i) (dec (long (mp/dimension-count m dim)))) 
        nil
        (error "Dimension index out of range: " i)))
    (labels [m dim]
      (if (<= 0 (long dim) (dec (long (mp/dimensionality m)))) 
        nil
        (error "Dimension out of range: " dim)))) 


;; =======================================================
;; default linear algebra implementations

(extend-protocol mp/PNorm
  Object
  (norm [m p]
    (cond
      (= p Double/POSITIVE_INFINITY) (mp/element-max m)
      (number? p) (mp/element-sum (mp/element-pow (mp/element-map m mops/abs) p))
      :else (error "p must be a number"))))

;; QR decomposition utility functions

(defn compute-q [m ^doubles qr-data mcols mrows min-len
                 ^doubles us ^doubles vs ^doubles gammas]
  (let [q ^doubles (mp/to-double-array (mp/identity-matrix vector mrows))]
    (c-for [i (dec min-len) (> i -1) (dec i)]
      (let [gamma (aget gammas i)]
        (aset us i 1.0)
        (c-for [j (inc i) (< j mrows) (inc j)]
          (aset us j
                (aget qr-data
                      (+ (* j mcols)
                         i))))
        (c-for [j i (< j mrows) (inc j)]
          (aset vs j
                (* (aget us i)
                   (aget q
                         (+ (* i mrows)
                            j)))))
        (c-for [j (inc i) (< j mrows) (inc j)]
          (let [u (aget us j)]
            (c-for [k i (< k mrows) (inc k)]
              (let [q-idx (+ (* j mrows)
                             i (- k i))]
                (aset vs k (+ (aget vs k)
                              (* u
                                 (aget q q-idx))))))))
        (c-for [j i (< j mrows) (inc j)]
          (aset vs j (* (aget vs j)
                        gamma)))

        (c-for [j i (< j mrows) (inc j)]
          (let [u (aget us j)]
            (c-for [k i (< k mrows) (inc k)]
              (let [qr-idx (+ (* j mrows)
                              i (- k i))]
                (aset q qr-idx (- (aget q qr-idx)
                                  (* u (aget vs k))))))))))
    (mp/compute-matrix m [mrows mrows]
                       (fn [i j]
                         (aget q (+ (* i mrows) j))))))



(defn compute-r [m ^doubles data mcols mrows min-len compact?]
  (let [cm (mp/compute-matrix
              m [mrows mcols]
              (fn [i j]
                (if (and (< i min-len)
                         (>= j i)
                         (< j mcols))
                  (aget data (+ (* i mcols) j))
                  0)))]
    (if compact?
      (->> (mp/get-major-slice-seq cm)
           (reduce
            #(if (every? zero? %2) (inc %1) %1) 0)
           (#(mp/reshape cm [mcols (- mrows %)])))
      cm)))

(defn householder-qr [^doubles qr-data idx mcols
                      mrows ^doubles us ^doubles gammas]
  (loop [qr-idx (+ idx (* idx mcols))
         i idx]
    (when (< i mrows)
      (aset us i (aget qr-data qr-idx))
      (recur (+ qr-idx mcols)
             (inc i))))
  (let [max_ (apply max (map #(Math/abs ^Double %)
                             (mp/subvector us idx (- mrows idx))))]
    (if (= max_ 0.0)
      {:error true}
      (let [_ (c-for [i idx (< i mrows) (inc i)]
                (aset us i (/ (aget us i) max_)))
            tau (->> (mp/subvector us idx (- mrows idx))
                     (map #(* % %))
                     (apply +)
                     (Math/sqrt))
            u-idx (aget us idx)
            tau (if (neg? u-idx) (- tau) tau)
            u-0 (+ u-idx tau)
            gamma (/ u-0 tau)
            tau (* tau max_)]
        (aset gammas idx gamma)
        (c-for [i (inc idx) (< i mrows) (inc i)]
          (aset us i (/ (aget us i) u-0)))
        (aset us idx 1.0)
        {:gamma gamma
         :gammas gammas
         :us us
         :tau tau
         :error false}))))

(defn update-qr [^doubles qr-data idx mcols mrows ^doubles vs
                 ^doubles us ^Double gamma ^Double tau]
  (let [u (aget us idx)
        idx+1 (inc idx)]
    (c-for [i idx+1 (< i mcols) (inc i)]
      (aset vs i (aget qr-data
                       (+ i
                          (* idx mcols)))))
    (c-for [i idx+1 (< i mrows) (inc i)]
      (let [qr-idx (+ idx+1
                      (* i mcols))]
        (c-for [j idx+1 (< j mcols) (inc j)]
          (aset vs j
                (+ (aget vs j)
                   (* (aget us i)
                      (aget qr-data (+ qr-idx
                                       (- j idx+1)))))))))
    (c-for [i idx+1 (< i mcols) (inc i)]
      (aset vs i (* (aget vs i)
                    gamma)))

    (c-for [i idx (< i mrows) (inc i)]
      (let [u (aget us i)]
        (c-for [j idx+1 (< j mcols) (inc j)]
          (let [qr-idx (+ (* i mcols)
                          idx+1
                          (- j idx+1))]
            (aset qr-data qr-idx
                  (- (aget qr-data qr-idx) (* u (aget vs j))))))))

    (when (< idx mcols)
      (aset qr-data (+ idx (* idx mcols)) (double (- tau))))

    (c-for [i idx+1 (< i mrows) (inc i)]
      (aset qr-data
            (+ idx (* i mcols))
            (aget us i)))
    {:qr-data qr-data
     :vs vs}))


(extend-protocol mp/PQRDecomposition
  Object
  (qr [m options]
    (let [[mrows mcols] (mp/get-shape m)
          min-len (min mcols mrows)
          max-len (max mcols mrows)]
      (loop [qr-data (mp/to-double-array m)
             vs (double-array max-len)
             us (double-array max-len)
             gammas (double-array min-len)
             gamma 0.0
             tau 0.0
             i 0]
        (if (< i min-len)
          (let [{:keys [us gamma gammas tau error]}
                (householder-qr
                 qr-data i mcols
                 mrows us gammas)]
            (when-not error
              (let [{:keys [qr-data vs]}
                    (update-qr qr-data i mcols mrows
                               vs us gamma tau)]
                (recur qr-data vs us gammas
                       (double gamma) (double tau) (inc i)))))
          (->>
           (select-keys
            {:Q #(compute-q m qr-data mcols mrows
                            min-len us vs gammas)
             :R #(compute-r m qr-data mcols mrows min-len (:compact options))}
            (:return options))
           (map (fn [[k v]] [k (v)]))
           (into {})))))))

;; temp var to prevent recursive coercion if implementation does not support liear algebra operation
(def ^:dynamic *trying-current-implementation* nil)

(defmacro try-current-implementation
  [sym form]
  `(if *trying-current-implementation*
     (TODO (str "Not yet implemented: " ~(str form) " for " (class ~sym)))
     (binding [*trying-current-implementation* true]
       (let [imp# (imp/get-canonical-object)
             ~sym (mp/coerce-param imp# ~sym)]
         ~form))))

(extend-protocol mp/PCholeskyDecomposition
  Object
  (cholesky [m options]
    (try-current-implementation m (mp/cholesky m options))))

(extend-protocol mp/PLUDecomposition
  Object
  (lu [m options]
    (try-current-implementation m (mp/lu m options))))

(extend-protocol mp/PSVDDecomposition
  Object
  (svd [m options]
    (try-current-implementation m (mp/svd m options))))

(extend-protocol mp/PEigenDecomposition
  Object
  (eigen [m options]
    (try-current-implementation m (mp/eigen m options))))

(extend-protocol mp/PSolveLinear
  Object
  (solve [a b]
    (try-current-implementation a (mp/solve a b))))

(extend-protocol mp/PLeastSquares
  Object
  (least-squares [a b]
    (try-current-implementation a (mp/least-squares a b))))

;; =======================================================
;; default multimethod implementations

(defmethod mm/mul :default [x y]
  (mp/inner-product x y))
