(ns clojure.core.matrix.impl.defaults
  "Default implementations for core.matrix protocols

   These should be correct reference implementations for all protocols that work on
   arbitrary objects. They are not necessarily tuned for performance.

   Default implementations are defined for:
    - nil (treated as a scalar nil value)
    - Numbers (treated as scalar numerical values)
    - Arbitrary arrays for which the protocol is not otherwise defined
  "
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.impl.mathsops :as mops :refer [to-degrees* to-radians*]]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.double-array :as da]
            [clojure.core.matrix.impl.common :refer [logistic-fn softplus-fn relu-fn construct-matrix
                                                     square? symmetric-matrix-entries? mapmatrix]]
            [clojure.core.matrix.utils :as u])
  #?@(:clj [(:require
              [clojure.core.matrix.macros :refer [TODO error scalar-coerce c-for doseq-indexed array?]]
              [clojure.core.matrix.macros-clj :refer [try-current-implementation eps== native-array?]])
            (:import [clojure.lang ISeq])]
      :cljs [(:require-macros
               [clojure.core.matrix.impl.defaults :refer [def-PMathsFunctions def-PMathsFunctionsMutable]]
               [clojure.core.matrix.macros :refer [TODO error scalar-coerce c-for doseq-indexed array?]]
               [clojure.core.matrix.macros-cljs :refer [try-current-implementation eps== native-array?]])]))

(def ^:dynamic *trying-current-implementation* nil)

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

#? (:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; (set! *unchecked-math* :warn-on-boxed) ;; use to check for boxing

;; ============================================================
;; Utility functions for default implementations
))

(defn- calc-element-count
  "Returns the total count of elements in an array"
  ([m]
    (if-let [sh (mp/get-shape m)]
      (reduce * sh)
      1)))

;; TODO: make smarter for different numeric types
;; TODO: have this return ndarrays once we have cljs support
(defn construct-mutable-matrix
  "Constructs a new mutable matrix with the given data."
  ([m]
    (let [dims (long (mp/dimensionality m))
          type (mp/element-type m)
          double? #?(:clj (or (= Double/TYPE type) (= java.lang.Double type))
                     :cljs (= js/Number type))]
      (cond
        (== dims 0)
          (wrap/wrap-scalar (mp/get-0d m))
        (and (== dims 1) double?)
          #?(:clj (da/construct-double-array m)
             :cljs (mp/coerce-param (imp/get-canonical-object :aljabr) m))
        double?
          (mp/coerce-param (imp/get-canonical-object #?(:clj :ndarray-double :cljs :aljabr)) m)
        :else
          (mp/coerce-param (imp/get-canonical-object #?(:clj :ndarray :cljs :aljabr)) m)))))

;; ============================================================
;; Default implementations
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise (java.lang.Object) we should try to
;;   implement in terms of simpler operations, on assumption that
;;   we have fallen through to the default implementation

;; default overall implementation

(extend-protocol mp/PImplementation
  #?(:clj Object :cljs object)
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

  ;; keyword implementation looks up the underlying implementation by keyword
  ;; this is intended to allow keywords to be used for dispatch rather than concrete
  ;; instances from the underlying implementation, e.g. (coerce :vectorz some-other-array)
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core.Keyword)
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
  #?(:clj Object :cljs object)
    (sparse-coerce [m data]
      nil) ;; allow fall through if sparse coercion is not directly supported
    (sparse [m]
      m))

(extend-protocol mp/PNative
  nil
    (native [m]
      nil)
    (native? [m] false)
  #?(:clj Object :cljs object)
    (native [m]
      nil) ;; allow fall through if native coercion is not directly supported
    (native? [m]
      false))

(extend-protocol mp/PNewSparseArray
  #?(:clj Object :cljs object)
    (new-sparse-array [m shape]
      ;; we don't support sparse arrays by default, so just return nil
      nil))

(extend-protocol mp/PDense
  nil
    (dense-coerce [m data]
      (mp/dense data))
    (dense [m]
      nil)
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
    (get-1d [m x]
      (error "Can't do 1D get on a scalar number"))
    (get-2d [m x y]
      (error "Can't do 2D get on a scalar number"))
    (get-nd [m indexes]
      (if-let [s (seq indexes)]
        (error "Can't do ND get on a scalar number with indexes: " s)
        m))
  #?(:clj Object :cljs object)
    (get-1d [m x]
      (cond
        (native-array? m) (mp/get-0d (nth m x))
        :else (mp/get-nd m [x])))
    (get-2d [m x y]
      (cond
        (native-array? m) (mp/get-1d (nth m x) y)
        :else (mp/get-nd m [x y])))
    (get-nd [m indexes]
      (if (seq indexes)
        (cond
          (native-array? m) (mp/get-nd (nth m (first indexes)) (next indexes))
          :else (error "Indexed get failed, not defined for:" (#?(:clj class :cljs type) m)))
        (mp/get-0d m)))

#?@(:cljs
     [cljs.core/LazySeq
      (get-1d [m x] (nth m x))
      (get-2d [m x y]
        (if (seqable? (first m))
          (mp/get-1d (nth m x) y)
          (error "Can't do 2D get on a lazy seq")))
      (get-nd [m indexes]
        (if (seq indexes)
          (mp/get-nd (nth m (first indexes)) (next indexes))
          (mp/get-0d m)))

      cljs.core/Range
      (get-1d [m x] (nth m x))
      (get-2d [m x y]
        (error "Can't do 2D get on a range"))
      (get-nd [m indexes]
              (if (seq indexes)
                (mp/get-nd (nth m (first indexes)) (next indexes))
                (mp/get-0d m)))
      ]
))

(extend-protocol mp/PArrayMetrics
  nil
    (nonzero-count [m] 1)
  #?(:clj Number :cljs number)
    (nonzero-count [m] (if (zero? m) 0 1)) ;; not possible to remove boxing warning
  #?(:clj Object :cljs object)
    (nonzero-count [m]
      (mp/element-reduce m (fn [cnt e] (if (zero? e) cnt (inc cnt))) 0))) ;; not possible to remove boxing warning

(extend-protocol mp/PZeroDimensionConstruction
  nil
    (new-scalar-array
      ([m] 0.0)
      ([m value]
        (wrap/wrap-scalar value)))
  #?(:clj Object :cljs object)
    (new-scalar-array
      ([m] (wrap/wrap-scalar 0.0))
      ([m value] (wrap/wrap-scalar value))))

(extend-protocol mp/PZeroDimensionAccess
  nil
    (get-0d [m]
      nil)
    (set-0d! [m value]
      (error "Can't set the value of nil!"))
  #?(:clj String :cljs string)
    (get-0d [m]
      m)
    (set-0d! [m value]
      (error "Can't set a string value!"))
  #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword)
    (get-0d [m]
      m)
    (set-0d! [m value]
      (error "Can't set a keyword!"))
  #?(:clj Number :cljs number)
    (get-0d [m]
      m)
    (set-0d! [m value]
      (error "Can't set a scalar number!"))
  #?(:clj Object :cljs object)
    (get-0d [m]
      ;; assume this is a scalar value
      m)
    (set-0d! [m value]
      (mp/set-nd! m [] value)))

(extend-protocol mp/PZeroDimensionSet
  nil
    (set-0d [m value]
      value ;; should be OK, since scalars satisfy 0d array abstraction
      )
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
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
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
    (numerical? [m]
      true)
  nil
    (numerical? [m]
      false)
  #?(:clj Object :cljs object)
    (numerical? [m]
      (if (mp/is-scalar? m)
        false ;; it's a scalar but not a number, so we do not recognise it as numerical
        (every? number? (mp/element-seq m)))))

(extend-protocol mp/PVectorOps
  #?(:clj Number :cljs number)
    (vector-dot [a b] (mp/pre-scale b a))
    (length [a] (Math/abs (double a)))
    (length-squared [a] (let [a (double a)] (* a a)))
    (normalise [a]
      (let [a (double a)]
        (cond
          (> a 0.0) 1.0
          (< a 0.0) -1.0
          :else 0.0)))
  #?(:clj Object :cljs object)
    (vector-dot [a b]
      ;; compute dot product if we have two vectors, otherwise return nil
      (when (and (== 1 (long (mp/dimensionality a))) (== 1 (long (mp/dimensionality b))))
        (mp/element-sum (mp/element-multiply a b))))
    (length [a]
      (Math/sqrt (double (mp/length-squared a))))
    (length-squared [a]
      (mp/element-reduce a (fn [^double r ^double x] (+ r (* x x))) 0.0))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PVectorDistance
  #?(:clj Number :cljs number)
    (distance [a b]
      (if (number? b)
        (Math/abs (- (double b) (double a)))
        (mp/distance b a)))
  #?(:clj Object :cljs object)
    (distance [a b] (double (mp/length (mp/matrix-sub a b)))))

(extend-protocol mp/PVectorCross
  #?(:clj Object :cljs object)
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
  #?(:clj Object :cljs object)
    (normalise! [a]
      (mp/scale! a (/ 1.0 (Math/sqrt (double (mp/length-squared a)))))))

(extend-protocol mp/PAssignment
  #?(:clj Object :cljs object)
    (assign! [m x]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (== 0 dims) (mp/set-0d! m (mp/get-0d x))
          (== 1 dims)
            (if (instance? ISeq x)
              ;; specialised handling for sequence (since indexed access would be O(n^2))
              (let [x (seq x)
                    msize (long (mp/dimension-count m 0))]
                (loop [i 0 s (seq x)]
                  (if (>= i msize)
                    (when s (error "Mismatches size of sequence in assign!"))
                    (do
                      (mp/set-1d! m i (first s))
                      (recur (inc i) (next s))))))
              ;; otherwise use indexed access
              (let [xdims (long (mp/dimensionality x))
                    msize (long (mp/dimension-count m 0))]
                (cond
                  (== 0 xdims)
                    (let [value (mp/get-0d x)]
                      (dotimes [i msize] (mp/set-1d! m i value)))
                  (== 1 xdims)
                    (do
                      (when (not= msize (long (mp/dimension-count x 0))) (error "Mismatched shapes in assign to array of shape: " (mp/get-shape m) " with argument of shape: " (mp/get-shape x)))
                      (dotimes [i msize] (mp/set-1d! m i (mp/get-1d x i))))
                  :else
                    (error "Can't assign! with an argument of higher dimensionality"))))

          (array? m)
            (let [xdims (long (mp/dimensionality x))]
              (if (== dims xdims)
                (let [xss (mp/get-major-slice-seq x)
                      _ (or (mp/same-shapes? xss) (error "Inconsistent slice shapes for assign!"))]
                  (doall (map (fn [a b] (mp/assign! a b)) (mp/get-major-slice-view-seq m) xss)))
                (doseq [ms (mp/get-major-slice-view-seq m)] (mp/assign! ms x))))
           :else
              (error "Can't assign to a non-array object: " (#?(:clj class :cljs type) m)))))
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
  #?(:clj Number :cljs number)
  (assign [m source]
    source)
  #?(:clj Object :cljs object)
    (assign [m source]
      (let [r (mp/broadcast-coerce m source)]
        (if (identical? r source) (mp/clone r) r))))

(extend-protocol mp/PMutableFill
  #?(:clj Object :cljs object)
    (fill! [m value]
      (mp/assign! m value)))

(extend-protocol mp/PMatrixCloning
   nil
     (clone [m]
       m)
   #?(:clj Number :cljs number)
     (clone [m]
       m)
   #?(:clj Object :cljs object)
     (clone [m]
       (mp/construct-matrix m m)))

(extend-protocol mp/PSparseArray
   #?(:clj Object :cljs object)
     (is-sparse? [m]
       false))

(extend-protocol mp/PImmutableMatrixConstruction
  nil
    (immutable-matrix [m]
      nil)
  #?(:clj Object :cljs object)
    (immutable-matrix [m]
      (if (mp/is-mutable? m)
        (mp/convert-to-nested-vectors m)
        m)))

(extend-protocol mp/PZeroCount
  nil
    (zero-count [m]
      0)
  #?(:clj Number :cljs number)
     (zero-count [m]
       ;; not possible to remove boxing warning, m may be any numeric type
       (if (zero? m) 1 0))
  #?(:clj Object :cljs object)
     (zero-count [m]
       ;; not possible to remove boxing warning, m may be any numeric type
       (mp/element-reduce m (fn [acc e] (if (zero? e) (inc acc) acc)) 0)))


(extend-protocol mp/PMutableMatrixConstruction
  nil
    (mutable-matrix [m]
      (wrap/wrap-scalar m))
  #?(:clj Number :cljs number)
    (mutable-matrix [m]
      (wrap/wrap-scalar m))
  #?(:clj Object :cljs object)
    (mutable-matrix [m]
      (construct-mutable-matrix m)))

(extend-protocol mp/PMutableCoercion
  nil
    (ensure-mutable [m]
      (wrap/wrap-scalar m))
  #?(:clj Number :cljs number)
    (ensure-mutable [m]
      (wrap/wrap-scalar m))
  #?(:clj Object :cljs object)
    (ensure-mutable [m]
      (if (mp/is-mutable? m)
        m
        (construct-mutable-matrix m))))

(extend-protocol mp/PComputeMatrix
  #?(:clj Object :cljs object)
    (compute-matrix [m shape f]
      (let [m (mp/new-matrix-nd m shape)]
        (reduce (fn [m ix] (mp/set-nd m ix (apply f ix))) m (u/base-index-seq-for-shape shape)))))

(extend-protocol mp/PDimensionInfo
  nil
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "nil has zero dimensionality, cannot get count for dimension: " i))
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core.Keyword)
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "Keyword has zero dimensionality, cannot get count for dimension: " i))
  #?(:clj String :cljs string)
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "String has zero dimensionality, cannot get count for dimension: " i))
  #?(:clj Number :cljs number)
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false)
    (get-shape [m] nil)
    (dimension-count [m i] (error "Number has zero dimensionality, cannot get count for dimension: " i))
  #?(:clj Object :cljs object)
    (dimensionality [m]
      (cond
        #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m)))
          (let [n (long (count m))]
            (if (> n 0) (inc (long (mp/dimensionality (nth m 0)))) 1))
        :else 0))
    (is-vector? [m]
      (cond
        #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m)))
          (let [n (long (count m))]
            (or (== n 0) (== 0 (long (mp/dimensionality (nth m 0))))))
        :else false))
    (is-scalar? [m]
      (cond
        #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m))) false ;; Java arrays are core.matrix arrays
        :else true)) ;; assume objects are scalars unless told otherwise
    (get-shape [m]
      (cond
        #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m)))
          (let [n (count m)]
            (if (== n 0) [0] (cons n (mp/get-shape (nth m 0)))))
        :else nil))
    (dimension-count [m i]
      (let [i (long i)]
        (cond
          #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m)))
            (if (== i 0) (count m) (mp/dimension-count (nth m 0) (dec i)))
          (== 0 i)
            (count m)
          :else (error "Can't determine count of dimension " i " on Object: " (#?(:clj class :cljs type) m)))))

#?@(:cljs
     [cljs.core/List
      (dimensionality [m] (inc (mp/dimensionality (first m))))
      (is-vector? [m] (== 0 (mp/dimensionality (first m))))
      (is-scalar? [m] false)
      (get-shape [m] (cons (count m) (mp/get-shape (first m))))
      (dimension-count [m x]
                       (if (== x 0)
                         (count m)
                         (mp/dimension-count (first m) (dec x))))

      cljs.core/LazySeq
      (dimensionality [m] (inc (mp/dimensionality (first m))))
      (is-vector? [m] (== 0 (mp/dimensionality (first m))))
      (is-scalar? [m] false)
      (get-shape [m] (cons (count m) (mp/get-shape (first m))))
      (dimension-count [m x]
                       (if (== x 0)
                         (count m)
                         (mp/dimension-count (first m) (dec x))))

      cljs.core/IndexedSeq
      (dimensionality [m] (inc (mp/dimensionality (first m))))
      (is-vector? [m] (== 0 (mp/dimensionality (first m))))
      (is-scalar? [m] false)
      (get-shape [m] (cons (count m) (mp/get-shape (first m))))
      (dimension-count [m x]
                       (if (== x 0)
                         (count m)
                         (mp/dimension-count (first m) (dec x))))

      cljs.core/Cons
      (dimensionality [m] (inc (mp/dimensionality (first m))))
      (is-vector? [m] (== 0 (mp/dimensionality (first m))))
      (is-scalar? [m] false)
      (get-shape [m] (cons (count m) (mp/get-shape (first m))))
      (dimension-count [m x]
                       (if (== x 0)
                         (count m)
                         (mp/dimension-count (first m) (dec x))))

      cljs.core/Range
      (dimensionality [m] (inc (mp/dimensionality (first m))))
      (is-vector? [m] (== 0 (mp/dimensionality (first m))))
      (is-scalar? [m] false)
      (get-shape [m] (cons (count m) (mp/get-shape (first m))))
      (dimension-count [m x]
                       (if (== x 0)
                         (count m)
                         (mp/dimension-count (first m) (dec x))))
      ]))

(extend-protocol mp/PSameShape
  nil
    (same-shape? [a b]
      (== 0 (long (mp/dimensionality b))))
  #?(:clj String :cljs string)
    (same-shape? [a b]
      (== 0 (long (mp/dimensionality b))))
  #?(:clj Number :cljs number)
    (same-shape? [a b]
      (== 0 (long (mp/dimensionality b))))
  #?(:clj Object :cljs object)
    (same-shape? [a b]
      (u/same-shape-object? (mp/get-shape a) (mp/get-shape b))))

;; generic versions of matrix ops
(extend-protocol mp/PMatrixOps
  nil
    (trace [m] m)
  #?(:clj Number :cljs number)
    (trace [m] m)
    (inverse [m] (/ m))
  #?(:clj Object :cljs object)
    (trace [m]
      (when-not (== 2 (long (mp/dimensionality m))) (error "Trace requires a 2D matrix"))
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
  #?(:clj Number :cljs number)
    (transpose [m] m)
  #?(:clj Object :cljs object)
    (transpose [m]
      (mp/coerce-param
       m
       (let [dims (long (mp/dimensionality m))]
         (case dims
           0 m
           1 m
           2 (apply mapv vector (mp/convert-to-nested-vectors m))
           (mp/transpose-dims m (reverse (range dims))))))))

(extend-protocol mp/PTransposeDims
  nil
    (transpose-dims [m ordering] m)
  #?(:clj Number :cljs number)
    (transpose-dims [m ordering] m)
  #?(:clj Object :cljs object)
    (transpose-dims [m ordering]
      (mp/transpose-dims (mp/convert-to-nested-vectors m) ordering)))

(extend-protocol mp/PTransposeInPlace
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
    (rotate [m dim places] m)
  #?(:clj Object :cljs object)
    (rotate [m dim places]
      (let [dim (long dim)
            places (long places)]
        (cond
          (<= (long (mp/dimensionality m)) 0)
            m
          (== 0 dim)
            (let [ss (mp/get-major-slice-seq m)
                  c (long (mp/dimension-count m 0))
                  sh (long (if (pos? c) (long (mod places c)) 0))]
              (if (== sh 0)
                m
                (vec (concat (take-last (- c sh) ss) (take sh ss)))))
         :else
           (mp/rotate (mp/convert-to-nested-vectors m) dim places)))))


(extend-protocol mp/PRotateAll
  nil
    (rotate-all [m shifts] nil)
  #?(:clj Number :cljs number)
    (rotate-all [m shifts] m)
  #?(:clj Object :cljs object)
    (rotate-all [m shifts]
      (reduce (fn [m [^long dim ^long shift]] (if (zero? shift) m (mp/rotate m dim shift)))
         m
         (map-indexed (fn [i v] [i v]) shifts))))

(extend-protocol mp/PShift
  #?(:clj Object :cljs object)
    (shift [m dim shift]
      (let [shift (long shift)
            z (mp/generic-zero m)
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
      (reduce (fn [m [dim ^long shift]] (if (zero? shift) m (mp/shift m dim shift)))
         m
         (map-indexed (fn [i v] [i v]) shifts))))


(extend-protocol mp/POrder
  nil
    (order
      ([m indices] (error "Can't reorder a scalar nil"))
      ([m dim indices] (error "Can't reorder a scalar nil")))
  #?(:clj Number :cljs number)
    (order
      ([m indices] (error "Can't reorder a scalar number"))
      ([m dim indices] (error "Can't reorder a scalar number")))
  #?(:clj Object :cljs object)
    (order
      ([m indices]
        (let [mshape (vec (mp/get-shape m))
              subshape (assoc m 0 1)
              ss (map #(mp/broadcast (mp/get-major-slice m %) subshape) indices)]
          (reduce #(mp/join %1 %2) ss)))
      ([m dim indices]
        (mp/order (mp/convert-to-nested-vectors m) dim indices))))

(defn- output-rank
  "Outputs a vector containing the rank of array elements, given a sorted sequence of [index value] pairs and a length"
  ([pairs n]
    (let [^objects dest (object-array n)]
      (loop [i (long 0)
             pairs (seq pairs)]
        (when (< i n)
          (aset dest (first (first pairs)) (Long/valueOf i))
          (recur (inc i)
                 (next pairs))))
      (vec dest))))

(extend-protocol mp/PIndexRank
  #?(:clj Object :cljs object)
    (index-rank
      ([m]
        (let [dims (long (mp/dimensionality m))]
          (case dims
            0 (error "Can't get indexed rank of a scalar value")
            1 (let [n (long (mp/element-count m))]
                (output-rank (sort-by second (map vector (range (mp/element-count m)) (mp/element-seq m))) n))
            (mapv mp/index-rank (mp/get-major-slice-seq m)))))
      ([m comp]
        (let [dims (long (mp/dimensionality m))]
          (case dims
            0 (error "Can't get indexed rank of a scalar value")
            1 (let [n (long (mp/element-count m))]
                (output-rank (sort-by second comp (map vector (range (mp/element-count m)) (mp/element-seq m))) n))
            (mapv #(mp/index-rank % comp) (mp/get-major-slice-seq m)))))))

;; not possible to remove boxing warning, may be any numeric type
(extend-protocol mp/PMatrixProducts
  #?(:clj Number :cljs number)
    (inner-product [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
    (outer-product [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
  #?(:clj Object :cljs object)
    (inner-product [m a]
      (cond
        (mp/is-scalar? m)
          (mp/pre-scale a m)
        (mp/is-scalar? a)
          (mp/scale m a)
        (== 1 (long (mp/dimensionality m)))
          (if (== 1 (long (mp/dimensionality a)))
            (mp/element-sum (mp/element-multiply m a))
            (reduce mp/matrix-add (map (fn [sl x] (mp/scale sl x))
                                       (mp/get-major-slice-seq a)
                                       (mp/get-major-slice-seq m)))) ;; TODO: implement with mutable accumulation
        :else
        (mp/construct-matrix (imp/get-canonical-object) (map #(mp/inner-product % a) (mp/get-major-slice-seq m)))))
    (outer-product [m a]
      (cond
        (mp/is-scalar? m)
          (mp/pre-scale a m)
        :else
        (mp/element-map (mp/convert-to-nested-vectors m) (fn [x] (mp/pre-scale a x)))
        ;; convert to nested vectors first, this enables trick of extending dimensionality for each element with element-map
        ;(mp/element-map (mp/convert-to-nested-vectors m) (fn [v] (mp/pre-scale a v)))
        )))

;; matrix multiply
;; TODO: document returning NDArray
(extend-protocol mp/PMatrixMultiply
  #?(:clj Number :cljs number)
    (element-multiply [m a]
      (if (number? a)
        (clojure.core/* m a)
        (mp/pre-scale a m)))
    (matrix-multiply [m a]
      (cond
        (number? a) (* m a)
        (array? a) (mp/pre-scale a m)
        :else (error "Don't know how to multiply number with: " (#?(:clj class :cljs type) a))))
  #?(:clj Object :cljs object)
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
           (let [mshape (mp/get-shape m)
                 [mrows mcols] mshape
                 acount (mp/element-count a)]
             (when (not= mcols acount) (error "Can't multiply matrix of shape: " mshape " with a vector of length " acount))
             (mp/reshape (mp/matrix-multiply m (mp/reshape a [mcols 1]))
                         [mrows]))
         (and (== mdims 2) (== adims 2))
           (let [mutable (mp/is-mutable? m)
                 [^long mrows ^long mcols] (mp/get-shape m)
                 [^long arows ^long acols] (mp/get-shape a)
                 new-m-type (if mutable m (imp/get-canonical-object :ndarray))
                 new-m (mp/new-matrix new-m-type mrows acols)]
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
             new-m))))
    (element-multiply [m a]
      (if (number? a)
        (mp/scale m a)
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m clojure.core/* a)))))

;; matrix multiply
(extend-protocol mp/PMatrixMultiplyMutable
  #?(:clj Number :cljs number)
    (element-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
    (matrix-multiply! [m a]
      (error "Can't do mutable multiply on a scalar number"))
  #?(:clj Object :cljs object)
    (element-multiply! [m a]
      (mp/assign! m (mp/element-multiply m a)))
    (matrix-multiply! [m a]
      (mp/assign! m (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixDivide
  #?(:clj Number :cljs number)
    (element-divide
      ([m] (/ m))
      ([m a] (mp/pre-scale (mp/element-divide a) m)))
  #?(:clj Object :cljs object)
    (element-divide
      ([m]
        (if (mp/get-shape m)
          (mp/element-map m mp/element-divide)
          (error "Don't know how to take reciprocal of " (type m))))
      ([m a]
        (mp/element-multiply m (mp/element-divide a)))))

(extend-protocol mp/PMatrixDivideMutable
  #?(:clj Number :cljs number)
	  (element-divide!
	    ([m] (error "Can't do mutable divide on a scalar number"))
	    ([m a] (error "Can't do mutable divide on a scalar numer")))
  #?(:clj Object :cljs object)
	  (element-divide!
	    ([m] (mp/element-map! m /))
	    ([m a]
	       (let [[m a] (mp/broadcast-compatible m a)]
	         (mp/element-map! m / a)))))

;; matrix element summation
(extend-protocol mp/PSummable
  #?(:clj Number :cljs number)
    (element-sum [a] a)
  #?(:clj Object :cljs object)
    (element-sum [a]
      (mp/element-reduce a (if (mp/numerical? a) + mp/matrix-add))))

;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PElementMinMax
  #?(:clj Number :cljs number)
    (element-min [m] m)
    (element-max [m] m)
    (element-clamp [m a b]
      (if-not (<= a b)
        (error "min argument: " a " should be <= max argument: " b)
        (if (< m a) a (if (> m b) b m))))
  #?(:clj Object :cljs object)
    (element-min [m]
      (mp/element-reduce m
                       (fn [best v] (if (or (not best) (< v best)) v best))
                       nil))
    (element-max [m]
      (mp/element-reduce m
                       (fn [best v] (if (or (not best) (> v best)) v best))
                       nil))
    (element-clamp [m a b]
      (if-not (<= a b)
        (error "min argument: " a " should be <= max argument: " b)
        (mp/element-map m #(if (< %1 a) a (if (> %1 b) b %1))))))

(extend-protocol mp/PCompare
  #?(:clj Number :cljs number)
    (element-compare [a b]
      (if (number? b)
        (long (mops/signum (- a b)))
        (mp/signum (mp/matrix-sub a b))))
    (element-if [m a b]
      (let [[a b] (mp/broadcast-same-shape a b)]
        (if (> m 0) a b)))
    (element-lt [m a]
      (if (number? a)
        (if (< m a) 1 0)
        (mp/element-gt a m)))
    (element-le [m a]
      (if (number? a)
        (if (<= m a) 1 0)
        (mp/element-ge a m)))
    (element-gt [m a]
      (if (number? a)
        (if (> m a) 1 0)
        (mp/element-lt a m)))
    (element-ge [m a]
      (if (number? a)
        (if (>= m a) 1 0)
        (mp/element-le a m)))
    (element-ne [m a]
      (if (number? a)
        (if (not= m a) 1 0)
        (mp/element-ne a m)))
    (element-eq [m a]
      (if (number? a)
        (if (= m a) 1 0)
        (mp/element-eq a m)))
  #?(:clj Object :cljs object)
    (element-compare [a b]
      (mp/element-map (mp/matrix-sub a b) #(long (mops/signum %))))
    (element-if [m a b]
      (cond
        (and (number? a) (number? b))
          (mp/element-map m #(if (> %1 0) a b))
        (number? a)
          (mp/element-map m #(if (> %1 0) a %2) b)
        (number? b)
          (mp/element-map m #(if (> %1 0) %2 b) a)
        :else (mp/element-map m #(if (> %1 0) %2 %3) a [b])) ;; note we need [b] because this is a `more` argument
      )
    (element-lt [m a]
      (if (number? a)
        (mp/element-map m #(if (< %1 a) 1 0))
        (mp/element-map m #(if (< %1 %2) 1 0) a)))
    (element-le [m a]
      (if (number? a)
        (mp/element-map m #(if (<= %1 a) 1 0))
        (mp/element-map m #(if (<= %1 %2) 1 0) a)))
    (element-gt [m a]
      (if (number? a)
        (mp/element-map m #(if (> %1 a) 1 0))
        (mp/element-map m #(if (> %1 %2) 1 0) a)))
    (element-ge [m a]
      (if (number? a)
        (mp/element-map m #(if (>= %1 a) 1 0))
        (mp/element-map m #(if (>= %1 %2) 1 0) a)))
    (element-ne [m a]
      (if (number? a)
        (mp/element-map m #(if-not (== %1 a) 1 0))
        (mp/element-map m #(if-not (== %1 %2) 1 0) a)))
    (element-eq [m a]
      (if (number? a)
        (mp/element-map m #(if (== %1 a) 1 0))
        (mp/element-map m #(if (== %1 %2) 1 0) a))))

;; add-product operations
(extend-protocol mp/PAddProduct
  #?(:clj Number :cljs number)
    (add-product [m a b]
      (mp/matrix-add (mp/element-multiply a b) m ))
  #?(:clj Object :cljs object)
    (add-product [m a b]
      (mp/matrix-add m (mp/element-multiply a b))))

(extend-protocol mp/PAddProductMutable
  #?(:clj Number :cljs number)
    (add-product! [m a b]
      (error "Numbers are not mutable"))
  #?(:clj Object :cljs object)
    (add-product! [m a b]
      (mp/matrix-add! m (mp/element-multiply a b))))

(extend-protocol mp/PAddScaled
  #?(:clj Number :cljs number)
    (add-scaled [m a factor]
      (mp/matrix-add (mp/scale a factor) m))
  #?(:clj Object :cljs object)
    (add-scaled [m a factor]
      (mp/matrix-add m (mp/scale a factor))))

(extend-protocol mp/PAddScaledMutable
  #?(:clj Number :cljs number)
    (add-scaled! [m a factor]
      (error "Numbers are not mutable"))
  #?(:clj Object :cljs object)
    (add-scaled! [m a factor]
      (mp/matrix-add! m (mp/scale a factor))))

(extend-protocol mp/PAddScaledProduct
  #?(:clj Number :cljs number)
    (add-scaled-product [m a b factor]
      (mp/matrix-add (mp/scale (mp/element-multiply a b) factor) m))
  #?(:clj Object :cljs object)
    (add-scaled-product [m a b factor]
      (mp/matrix-add m (mp/scale (mp/element-multiply a b) factor))))

(extend-protocol mp/PAddScaledProductMutable
  #?(:clj Number :cljs number)
    (add-scaled-product! [m a b factor]
      (error "Numbers are not mutable"))
  #?(:clj Object :cljs object)
    (add-scaled-product! [m a b factor]
      (mp/matrix-add! m (mp/scale (mp/element-multiply a b) factor))))

;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PScaleAdd
  #?(:clj Object :cljs object)
    (scale-add! [m1 a m2 b constant]
      (mp/element-multiply! m1 a)
      (when-not (and (number? b) (zero? b)) (mp/add-product! m1 m2 b))
      (when-not (and (number? constant) (zero? constant)) (mp/matrix-add! m1 constant))
      m1))

(extend-protocol mp/PScaleAdd2
  #?(:clj Object :cljs object)
    (scale-add [m1 a m2 b constant]
      (let [r (mp/matrix-add (mp/scale m1 a) (mp/scale m2 b))]
        (if (== 0.0 constant)
          r
          (mp/matrix-add r constant)))))

(extend-protocol mp/PLerp
  #?(:clj Object :cljs object)
    (lerp [a b factor]
      (mp/scale-add a (- 1.0 (double factor)) b factor 0.0))
    (lerp! [a b factor]
      (mp/scale-add! a (- 1.0 (double factor)) b factor 0.0)))

(extend-protocol mp/PAddInnerProductMutable
  #?(:clj Object :cljs object)
    (add-inner-product!
      ([m a b]
        (mp/matrix-add! m (mp/inner-product a b)))
      ([m a b factor]
        (mp/add-scaled! m (mp/inner-product a b) factor))))

(extend-protocol mp/PAddOuterProductMutable
  #?(:clj Object :cljs object)
    (add-outer-product!
      ([m a b]
        (mp/matrix-add! m (mp/outer-product a b)))
      ([m a b factor]
        (mp/add-scaled! m (mp/outer-product a b) factor))))

(extend-protocol mp/PSetInnerProductMutable
  #?(:clj Object :cljs object)
    (set-inner-product!
      ([m a b]
        (mp/assign! m (mp/inner-product a b)))
      ([m a b factor]
        (mp/assign! m (mp/inner-product a b))
        (mp/scale! m factor))))

;; type of matrix element
;; the default is to assume any type is possible
(extend-protocol mp/PTypeInfo
  nil
    (element-type [a]
      #?(:clj Object :cljs js/Object))

  #?(:clj Object :cljs object)
    (element-type [a]
      (if (native-array? a)
        (.getComponentType (#?(:clj class :cljs type) a))
        #?(:clj Object :cljs js/Object))))

;; generic element values
(extend-protocol mp/PGenericValues
  #?(:clj Object :cljs object)
    (generic-zero [m]
      0)
    (generic-one [m]
      1)
    (generic-value [m]
      0))

;; general transformation of a vector
(extend-protocol mp/PVectorTransform
  #?(:clj clojure.lang.IFn
     :cljs cljs.core.IFn)
    (vector-transform [m a]
      (if
        (vector? m) (mp/matrix-multiply m a)
        (m a)))
    (vector-transform! [m a]
      (if
        (vector? m) (mp/assign! a (mp/matrix-multiply m a))
        (mp/assign! a (m a))))
  #?(:clj Object :cljs object)
    (vector-transform [m a]
      (cond
        (== 2 (long (mp/dimensionality m))) (mp/matrix-multiply m a)
        :else (error "Don't know how to transform using: " (#?(:clj class :cljs type) m))))
    (vector-transform! [m a]
      (mp/assign! a (mp/vector-transform m a))))

;; matrix scaling
;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PMatrixScaling
  #?(:clj Number :cljs number)
    (scale [m a]
      (if (number? a)
        (* m a)
        (mp/pre-scale a m)))
    (pre-scale [m a]
      (if (number? a)
        (* a m)
        (mp/scale a m)))
  #?(:clj Object :cljs object)
    (scale [m a]
      (mp/element-map m #(* % a)))
    (pre-scale [m a]
      (mp/element-map m (partial * a))))

;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PMatrixMutableScaling
  #?(:clj Number :cljs number)
    (scale! [m a]
      (error "Can't scale! a numeric value: " m))
    (pre-scale! [m a]
      (error "Can't pre-scale! a numeric value: " m))
  #?(:clj Object :cljs object)
    (scale! [m a]
      (mp/element-map! m #(* % a))
      m)
    (pre-scale! [m a]
      (mp/element-map! m (partial * a))
      m))

;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PMatrixAdd
  ;; matrix add for scalars
  #?(:clj Number :cljs number)
    (matrix-add [m a]
      (if (number? a)
        (+ m a)
        (mp/matrix-add a m)))
    (matrix-sub [m a]
      (if (number? a)
        (- m a)
        (mp/negate (mp/matrix-sub a m))))
  ;; default impelementation - assume we can use emap?
  #?(:clj Object :cljs object)
    (matrix-add [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m clojure.core/+ a)))
    (matrix-sub [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m clojure.core/- a))))



(extend-protocol mp/PMatrixAddMutable
  ;; matrix add for scalars
  #?(:clj Number :cljs number)
    (matrix-add! [m a]
      (error "Can't do mutable add! on a scalar number"))
    (matrix-sub! [m a]
      (error "Can't do mutable sub! on a scalar number"))
  ;; default impelementation - assume we can use emap?
  #?(:clj Object :cljs object)
    (matrix-add! [m a]
      (mp/element-map! m clojure.core/+ a))
    (matrix-sub! [m a]
      (mp/element-map! m clojure.core/- a)))

(extend-protocol mp/PNegation
  nil
    (negate [m]
      (error "Can't negate nil!"))
  #?(:clj Number :cljs number)
    (negate [m]
      (- m))
  #?(:clj Object :cljs object)
    (negate [m]
      (mp/scale m -1.0)))

;; equality checking
;; not possible to eliminate boxing warnings - needs to handle any numeric type
(extend-protocol mp/PMatrixEquality
  nil
    (matrix-equals [a b]
      (error "nil is not a valid numerical value in equality testing"))
  #?(:clj Number :cljs number)
    (matrix-equals [a b]
      (cond
        (number? b) (== a b)
        (== 0 (mp/dimensionality b)) (mp/matrix-equals b a) ;; defer to other implementation
        :else false))
  #?(:clj Object :cljs object)
    (matrix-equals [a b]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
        (if (== 0 (long (mp/dimensionality a)))
          (== (mp/get-0d a) (scalar-coerce b))
          (not-any? false? (map == (mp/element-seq a) (mp/element-seq b))))
        :else false)))

(extend-protocol mp/PValueEquality
  nil
    (value-equals [a b]
      (or
        (nil? b)
        (and
          (== 0 (long (mp/dimensionality b)))
          (nil? (mp/get-0d b)))))
  #?(:clj Number :cljs number)
  (value-equals [a b]
    (and
      (== 0 (long (mp/dimensionality b)))
      (== a (mp/get-0d b))))
  #?(:clj Object :cljs object)
    (value-equals [a b]
      (and
        (mp/same-shape? a b)
        (every? true? (map = (mp/element-seq a) (mp/element-seq b))))))

;; equality checking
(extend-protocol mp/PMatrixEqualityEpsilon
  nil
    (matrix-equals-epsilon [a b eps]
      (error "nil is not a valid numerical value in equality testing"))
  #?(:clj Number :cljs number)
    (matrix-equals-epsilon [a b eps]
      (cond
        (number? b) (eps== a b eps)
        (== 0 (long (mp/dimensionality b))) (eps== a (mp/get-0d b) eps)
        :else false))
  #?(:clj Object :cljs object)
    (matrix-equals-epsilon [a b eps]
      (cond
        (identical? a b) true
        (mp/same-shape? a b)
          (let [eps (double eps)]
            (every? #(<= (Math/abs (double %)) eps) (map - (mp/element-seq a) (mp/element-seq b))))
        :else false)))

(extend-protocol mp/PDoubleArrayOutput
  #?(:clj Number :cljs number)
    (to-double-array [m]
      (let [arr (double-array 1)] (aset arr 0 (double m)) arr))
    (as-double-array [m] nil)
  #?(:clj Object :cljs object)
    (to-double-array [m]
      (double-array (mp/element-seq m)))
    (as-double-array [m] nil))

(extend-protocol mp/PObjectArrayOutput
  nil
    (to-object-array [m]
      (let [arr (object-array 1)] arr))
    (as-object-array [m] nil)
  #?(:clj Number :cljs number)
    (to-object-array [m]
      (let [arr (object-array 1)] (aset arr 0 m) arr))
    (as-object-array [m] nil)
  #?(:clj Object :cljs object)
    (to-object-array [m]
      (object-array (mp/element-seq m)))
    (as-object-array [m] nil))

;; row operations
(extend-protocol mp/PRowOperations
  #?(:clj Object :cljs object)
    (swap-rows [m i j]
      (mp/swap-rows (mp/convert-to-nested-vectors m) i j))
    (multiply-row [m i k]
      (mp/multiply-row (mp/convert-to-nested-vectors m) i k))
    (add-row [m i j k]
      (mp/add-row (mp/convert-to-nested-vectors m) i j k)))

(extend-protocol mp/PRowSetting
  #?(:clj Object :cljs object)
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
  #?(:clj Object :cljs object)
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

;; slice-map
(extend-protocol mp/PSliceMap
  #?(:clj Object :cljs object)
  (slice-map
    ([m f]
      (construct-matrix m (mapv f (mp/get-major-slice-seq m))))
    ([m f a]
      (construct-matrix m (mapv f
                                (mp/get-major-slice-seq m)
                                (mp/get-major-slice-seq a))))
    ([m f a more]
      (construct-matrix m (apply mapv f
                                 (mp/get-major-slice-seq m)
                                 (mp/get-major-slice-seq a)
                                 (map mp/get-major-slice-seq more))))))

;; slice-map
(extend-protocol mp/PFilterSlices
  #?(:clj Object :cljs object)
  (filter-slices [m f]
    (let [slcs (filterv f (mp/get-major-slice-seq m))]
      ;; check for no slices, in which case we must return nil
      (if (seq slcs) slcs nil))))

(extend-protocol mp/PAddEmap
  #?(:clj Object :cljs object)
  (add-emap!
    ([dest f a]
      (mp/matrix-add! dest (mp/element-map a f)))
    ([dest f a b]
      (mp/matrix-add! dest (mp/element-map a f b)))
    ([dest f a b more]
      (mp/matrix-add! dest (mp/element-map a f b more)))))

(extend-protocol mp/PSetEmap
  #?(:clj Object :cljs object)
  (set-emap!
    ([dest f a]
      (mp/assign! dest (mp/element-map a f)))
    ([dest f a b]
      (mp/assign! dest (mp/element-map a f b)))
    ([dest f a b more]
      (mp/assign! dest (mp/element-map a f b more)))))

;; functional operations
(extend-protocol mp/PFunctionalOperations
  #?(:clj Number :cljs number)
    (element-seq [m]
      (vector m))
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
  #?(:clj Object :cljs object)
    (element-seq [m]
      (let [c (#?(:clj #?(:clj class :cljs type) :cljs type) m)
            dims (long (mp/dimensionality m))]
        (cond
          (> dims 1) (mapcat mp/element-seq (mp/get-major-slice-seq m))
          (seq? m) m
          (== 0 dims)
            (vector (mp/get-0d m))
 #?@(:clj [(and (.isArray c) (.isPrimitive (.getComponentType c))) m]
    :cljs [(= js/Array c) m])
          (== 1 dims)
            (mp/convert-to-nested-vectors m)
          (array? m)
            (mapcat mp/element-seq (mp/get-major-slice-seq m))
          :else (error "Don't know how to create element-seq from: " m))))
    (element-map
      ([m f]
        (construct-matrix m (mapmatrix f m)))
      ([m f a]
        (let [[m a] (mp/broadcast-same-shape m a)]
          (construct-matrix m (mapmatrix f m a))))
      ([m f a more]
        (let [arrays (cons m (cons a more))
              shapes (map mp/get-shape arrays)
              sh (or (mp/common-shape shapes) (error "Attempt to do element map with incompatible shapes: " (mapv mp/get-shape arrays)))
              arrays (map #(mp/broadcast % sh) arrays)]
          (construct-matrix m (apply mapmatrix f arrays)))))

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
      ([m f init] (f init nil)))

#?@(:cljs
  [cljs.core/List
    (element-seq [m]
     (cond
        (== 0 (count m))
          nil
        (>= (long (mp/dimensionality (nth m 0))) 1)
          ;; we are a 2D+ array, so be conservative and create a concatenated sequence
          (mapcat mp/element-seq m)
        :else
          ;; we are a 1D vector, so already a valid seqable result for element-seq
          m))])
    )

(defn- cart [colls]
  (if (empty? colls)
    [[]]
    (for [x    (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn- indices-seq [m]
  (cart (map range (mp/get-shape m))))

(extend-protocol mp/PMapIndexed
  #?(:clj Number :cljs number)
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
  #?(:clj Object :cljs object)
    (element-map-indexed
      ([m f]
        (if (== 0 (long (mp/dimensionality m)))
          (f [] (mp/get-0d m)) ;; handle case of single element
          (let [s (map f (indices-seq m) (mp/element-seq m))]
            (mp/reshape (mp/coerce-param m s)
                        (mp/get-shape m)))))
      ([m f a]
        (if (== 0 (long (mp/dimensionality m)))
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
  #?(:clj Number :cljs number) (element-count [m] 1)
  #?(:clj Object :cljs object)
    (element-count [m]
      (calc-element-count m)))

(extend-protocol mp/PValidateShape
  nil
    (validate-shape 
      ([m] nil)
      ([m shape] (when-not (nil? shape) (error "Shape validation failed, was scalar but expected: " shape))))
  #?(:clj Object :cljs object)
    (validate-shape 
      ([m]
        (mp/validate-shape m (mp/get-shape m)))
      ([m expected-shape]
        (cond
          (== 0 (long (mp/dimensionality m)))
            (let [sh (mp/get-shape m)]
              (if (nil? sh)
                (when-not (nil? expected-shape)
                  (error "Shape validation failed, was scalar but expected: " expected-shape))
                (if (= (vec sh) (vec expected-shape)) 
                  sh 
                  (error "Shape validation failed, was " sh " but expected: " expected-shape)))
              )
          :else
            (let [sh (mp/get-shape m)
                  ss (mp/get-major-slice-seq m)
                  efirst (or (first expected-shape) (error "Shape validation failed, was " sh " but expected: " expected-shape))
                  enext (next expected-shape) 
                  shapes (mapv #(mp/validate-shape % enext) ss)]
              (if (apply = enext shapes)
                (vec (cons (mp/dimension-count m 0) (first shapes)))
                (error "Inconsistent shapes for sub arrays in " (#?(:clj class :cljs type) m))))))))

(extend-protocol mp/PMatrixSlices
  #?(:clj Object :cljs object)
    (get-row [m i]
      (if (native-array? m)
        (nth m i)
        (mp/get-major-slice m i)))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (cond
        (native-array? m) (nth m i)
        (== 1 (long (mp/dimensionality m))) (mp/get-1d m i)
        :else (clojure.core.matrix.impl.wrappers/wrap-slice m i)))
    (get-slice [m dimension i]
      (let [ldimension (long dimension)]
        (cond
          (== 0 ldimension) (mp/get-major-slice m i)
          :else (mp/get-slice (mp/convert-to-nested-vectors m) dimension i)))))

(extend-protocol mp/PBLASBase
  #?(:clj Object :cljs object)
  (gemm! [c trans-a? trans-b? alpha a b beta]
    (let [a (if trans-a? (mp/transpose a) a)
          b (if trans-b? (mp/transpose b) b)]
      (if-not (== 1.0 (double beta)) (mp/scale! c beta))
      (mp/add-inner-product! c a b alpha)))
  (gemv! [c trans-a? alpha a b beta]
    (let [a (if trans-a? (mp/transpose a) a)]
      (if-not (== 1.0 (double beta)) (mp/scale! c beta))
      (mp/add-inner-product! c a b alpha))))

(extend-protocol mp/PMatrixColumns
  #?(:clj Object :cljs object)
  (get-columns [m]
    (case (long (mp/dimensionality m))
      0 (error "Can't get columns of a 0-dimensional object")
      1 (error "Can't get columns of a 1-dimensional object")
      2 (vec (mp/get-slice-seq m 1))
      (vec (mapcat mp/get-columns (mp/get-major-slice-seq m))))))

(extend-protocol mp/PMatrixRows
  #?(:clj Object :cljs object)
  (get-rows [m]
    (case (long (mp/dimensionality m))
      0 (error "Can't get rows of a 0-dimensional object")
      1 (error "Can't get rows of a 1-dimensional object")
      2 (vec (mp/get-major-slice-seq m))
      (vec (mapcat mp/get-rows (mp/get-major-slice-seq m))))))

(extend-protocol mp/PSliceView
  #?(:clj Object :cljs object)
    ;; default implementation uses a lightweight wrapper object
    (get-major-slice-view [m i]
      (cond
        (native-array? m)
          (let [ss (nth m i)]
            (if (array? ss)
              ss
              (clojure.core.matrix.impl.wrappers/wrap-slice m i)))
        :else (clojure.core.matrix.impl.wrappers/wrap-slice m i))))

(extend-protocol mp/PSliceView2
  #?(:clj Object :cljs object)
    (get-slice-view [m dim i]
      (if (zero? dim)
        (mp/get-major-slice-view m i)
        (mp/get-slice-view (clojure.core.matrix.impl.wrappers/wrap-nd m) dim i))))

(extend-protocol mp/PSliceSeq
  #?(:clj Object :cljs object)
    (get-major-slice-seq [m]
      (let [dims (long (mp/dimensionality m))]
        (cond
          (<= dims 0) (error "Can't get slices on [" dims "]-dimensional object")
          #?(:clj (.isArray (.getClass m)) :cljs (= js/Array (type m))) (seq m)
          (== dims 1) (for [i (range (mp/dimension-count m 0))] (mp/get-1d m i))
          :else (map #(mp/get-major-slice m %) (range (mp/dimension-count m 0)))))))

(extend-protocol mp/PSliceSeq2
  #?(:clj Object :cljs object)
    (get-slice-seq [m dimension]
      (let [ldimension (long dimension)]
        (cond
          (== ldimension 0) (mp/get-major-slice-seq m)
          (< ldimension 0) (error "Can't get slices of a negative dimension: " dimension)
          :else (map #(mp/get-slice m dimension %) (range (mp/dimension-count m dimension)))))))

(extend-protocol mp/PSliceViewSeq
  #?(:clj Object :cljs object)
    (get-major-slice-view-seq [m]
      (let [n (mp/dimension-count m 0)]
        (for [i (range n)]
          (mp/get-major-slice-view m i)))))

(extend-protocol mp/PSliceJoin
  nil
    (join [m a]
      (error "Can't join an array to a nil value!"))
  #?(:clj Number :cljs number)
    (join [m a]
      (error "Can't join an array to a scalar number!"))
  #?(:clj Object :cljs object)
    (join [m a]
      (let [dims (long (mp/dimensionality m))
            adims (long (mp/dimensionality a))]
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
  #?(:clj Number :cljs number)
  (join-along [m a dim]
    (error "Can't join an array to a scalar number!"))
  #?(:clj Object :cljs object)
  (join-along [m a dim]
    (mp/coerce-param m
      (let [dim (long dim)]
        (cond
          (== dim 0)
            (mp/join m a)
          :else
            (let [ddim (dec dim)]
              (mapv #(mp/join-along %1 %2 ddim)
                   (mp/get-major-slice-seq m)
                   (mp/get-major-slice-seq a))))))))

(extend-protocol mp/PSubVector
  nil
    (subvector [m start length]
      (error "Can't take subvector of nil"))
  #?(:clj Number :cljs number)
    (subvector [m start length]
      (error "Can't take subvector of a scalar number"))
  #?(:clj Object :cljs object)
    (subvector [m start length]
      (mp/subvector (wrap/wrap-nd m) start length)))

(extend-protocol mp/PSubMatrix
  nil
    (submatrix [m index-ranges]
      (if (seq index-ranges)
        (error "Can't take partial submatrix of nil")
        m))
  #?(:clj Number :cljs number)
    (submatrix [m index-ranges]
      (if (seq index-ranges)
        (error "Can't take partial submatrix of a scalar number")
        m))
  #?(:clj Object :cljs object)
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
  #?(:cljs number)
  #?(:cljs (broadcast [m new-shape]
                      (wrap/wrap-broadcast m new-shape)))

  #?(:clj Object :cljs object)
    (broadcast [m new-shape]
      (let [nshape new-shape
            mshape (mp/get-shape m)
            mdims (count mshape)
            ndims (count nshape)]
        (cond
          (and (== mdims ndims) (u/same-shape-object? nshape mshape)) m
          ;(and (> ndims mdims) (== mshape (drop (- ndims mdims) nshape)))
          ;  (let [rep (nth nshape (- ndims mdims 1))]
          ;    (mp/broadcast (vec (repeat rep m)) new-shape))
          :else (wrap/wrap-broadcast m new-shape)))))

(extend-protocol mp/PBroadcastLike
  nil
    (broadcast-like [m a]
      (wrap/wrap-broadcast a (mp/get-shape m)))
  #?(:clj Object :cljs object)
    (broadcast-like [m a]
      (let [sm (mp/get-shape m) sa (mp/get-shape a)]
        (if (u/same-shape-object? sm sa)
          a
          (mp/broadcast a sm)))))

(extend-protocol mp/PBroadcastCoerce
  nil
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a)))
  #?(:clj Object :cljs object)
    (broadcast-coerce [m a]
      (mp/coerce-param m (mp/broadcast-like m a))))

(extend-protocol mp/PPack
  nil
    (pack [m]
      nil)
  #?(:clj Object :cljs object)
    (pack [m]
      m))

;; attempt conversion to nested vectors
(extend-protocol mp/PConversion
  nil
    (convert-to-nested-vectors [m]
      nil)
  #?(:clj Number :cljs number)
    (convert-to-nested-vectors [m]
      ;; we accept a scalar as a "nested vector" for these purposes
      m)
  #?(:clj Object :cljs object)
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
          (array? m)
              (mapv mp/convert-to-nested-vectors (mp/get-major-slice-seq m))
          (seq? m)
              (mapv mp/convert-to-nested-vectors m)
          :default
              (error "Can't work out how to convert to nested vectors: " (#?(:clj class :cljs type) m) " = " m)))))

(extend-protocol mp/PRowColMatrix
  nil
    (column-matrix [m data] (error "Can't create a column matrix from nil"))
    (row-matrix [m data] (error "Can't create a column matrix from nil"))
  #?(:clj Object :cljs object)
    (column-matrix [m data]
      (if (== 1 (long (mp/dimensionality data)))
        (mp/coerce-param m (mapv vector (mp/element-seq data)))
        (error "Can't create a column matrix: input must be 1D vector")))
    (row-matrix [m data]
      (if (== 1 (long (mp/dimensionality data)))
        (mp/coerce-param m (vector data)) ;; i.e. just wrap in a
        (error "Can't create a row matrix: input must be 1D vector"))))

(extend-protocol mp/PVectorView
  nil
    (as-vector [m]
      [nil])
  #?(:clj Number :cljs number)
    (as-vector [m]
      [m])
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
    (to-vector [m]
      [m])
  #?(:clj Object :cljs object)
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
  #?(:clj Number :cljs number)
    (reshape [m shape]
      (mp/reshape [m] shape))
  #?(:clj Object :cljs object)
    (reshape [m shape]
      (if (= (mp/get-shape m) shape) ;; Short circuit if already the desired shape
        m
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
              (or
                (mp/construct-matrix m (take fs parts))
                (mp/construct-matrix [] (take fs parts))))
            (first es))))))

(extend-protocol mp/PReshapeView
  nil
    (reshape-view [m shape]
      (mp/broadcast nil shape))
  #?(:clj Number :cljs number)
    (reshape-view [m shape]
      (mp/broadcast m shape))
  #?(:clj Object :cljs object)
    (reshape-view [m shape]
      (if (mp/is-mutable? m)
        (TODO "reshape-view not supported on mutable array of type: " (class m))
        (mp/reshape m shape))))

(extend-protocol mp/PCoercion
  nil
    (coerce-param [m param]
      param)
  #?(:clj Number :cljs number)
    (coerce-param [m param]
      param)
  #?(:clj Object :cljs object)
    (coerce-param [m param]
      ;; NOTE: leave param unchanged if coercion not possible (probably an invalid shape for implementation)
      (let [param (if (instance? ISeq param) (mp/convert-to-nested-vectors param) param)] ;; ISeqs can be slow, so convert to vector first
        (or (mp/construct-matrix m param)
           param))))

(extend-protocol mp/PExponent
  #?(:clj Number :cljs number)
  (element-pow [m exponent]
    #?(:clj
        (if (array? exponent)
          (mp/element-map exponent #(Math/pow (.doubleValue m) (.doubleValue ^Number %)))
          (Math/pow (.doubleValue m) (double exponent)))
       :cljs
        (if (array? exponent)
          (mp/element-map exponent #(Math/pow m %))
          (Math/pow m exponent))))
  #?(:clj Object :cljs object)
  (element-pow [m exponent]
    #?(:clj
        (if (array? exponent)
          (mp/element-map m #(Math/pow (.doubleValue ^Number %1) (.doubleValue ^Number %2)) exponent)
          (mp/element-map m #(Math/pow (.doubleValue ^Number %) exponent)))
       :cljs
        (if (array? exponent)
          (mp/element-map m #(Math/pow %1 %2) exponent)
          (mp/element-map m #(Math/pow % exponent))))))

(extend-protocol mp/PSquare
  #?(:clj Number :cljs number)
   (square [m] (* m m)) ;; can't eliminate boxing warning, may be any numerical type
  #?(:clj Object :cljs object)
   (square [m] (mp/element-multiply m m)))

(extend-protocol mp/PLogistic
  #?(:clj Number :cljs number)
    (logistic [m]
      (let [e-t (Math/exp (- (double m)))]
        (/ 1.0 (+ 1.0 e-t))))
  #?(:clj Object :cljs object)
    (logistic [m]
      (mp/element-map m logistic-fn)))

(extend-protocol mp/PLogisticMutable
  #?(:clj Object :cljs object)
    (logistic! [m]
      (mp/element-map! m logistic-fn)))

(extend-protocol mp/PSoftplus
  #?(:clj Number :cljs number)
    (softplus [m]
      (let [et (Math/exp (double m))]
        (Math/log (+ 1.0 et))))
  #?(:clj Object :cljs object)
    (softplus [m]
      (mp/element-map m softplus-fn)))

(extend-protocol mp/PSoftmax
  #?(:clj Object :cljs object)
    (softmax [m]
      (let [em (mp/exp m)]
        (mp/element-divide em (mp/element-sum em)))))

(extend-protocol mp/PSoftmaxMutable
  #?(:clj Object :cljs object)
    (softmax! [m]
      (mp/exp! m)
      (mp/element-divide! m (mp/element-sum m))
      m))

(extend-protocol mp/PSoftplusMutable
  #?(:clj Object :cljs object)
    (softplus! [m]
      (mp/element-map! m softplus-fn)))

(extend-protocol mp/PReLU
  #?(:clj Number :cljs number)
    (relu [m]
      (Math/max 0.0 (double m)))
  #?(:clj Object :cljs object)
    (relu [m]
      (mp/element-map m relu-fn)))

(extend-protocol mp/PReLUMutable
  #?(:clj Object :cljs object)
    (relu! [m]
      (mp/element-map! m relu-fn)))


#?(:clj  (do

(defmacro def-PMathsFunctions
  [clj?]
 `(extend-protocol mp/PMathsFunctions
    ~(if clj? 'Number 'number)
    ~@(map (fn [[name func cljs-func]]
             (let [func (if (and cljs-func (not clj?)) cljs-func func)]
               `(~name [~'m] (~'double (~func (~'double ~'m))))))
        `~mops/maths-ops)

    ~(if clj? 'Object 'object)
    ~@(map (fn [[name func cljs-func]]
             (let [func (if (and cljs-func (not clj?)) cljs-func func)]
               `(~name [~'m] (mp/element-map ~'m #(~'double (~func (~'double %)))))))
        `~mops/maths-ops)

    ~@(when (not clj?)
      `[~'array
        ~@(map (fn [[name func cljs-func]]
                 (let [func (if (and cljs-func (not clj?)) cljs-func func)]
                   `(~name [~'m] (mp/element-map ~'m #(~'double (~func (~'double %)))))))
               `~mops/maths-ops)
       ]))
 )

(defmacro def-PMathsFunctionsMutable
  [clj?]
  `(extend-protocol mp/PMathsFunctionsMutable
    ~(if clj? 'Number 'number)
    ~@(map (fn [[name func cljs-func]]
             (let [func (if (and cljs-func (not clj?)) cljs-func func)]
               `(~(symbol (str name "!")) [~'m] (error "Number is not mutable!"))))
        `~mops/maths-ops)

    ~(if clj? 'Object 'object)
    ~@(map (fn [[name func cljs-func]]
             (let [func (if (and cljs-func (not clj?)) cljs-func func)]
               `(~(symbol (str name "!")) [~'m] (mp/element-map! ~'m #(~'double (~func (~'double %)))))))
        `~mops/maths-ops)

    ~@(when (not clj?)
      `[~'array
        ~@(map (fn [[name func cljs-func]]
                 (let [func (if (and cljs-func (not clj?)) cljs-func func)]
                   `(~(symbol (str name "!")) [~'m] (mp/element-map! ~'m #(~'double (~func (~'double %)))))))
               `~mops/maths-ops)
      ])
    ))
))

(def-PMathsFunctions #?(:clj true :cljs false))
(def-PMathsFunctionsMutable #?(:clj true :cljs false))

(extend-protocol mp/PMatrixSubComponents
  #?(:clj Object :cljs object)
    (main-diagonal [m]
      (let [sh (mp/get-shape m)
            rank (count sh)
            dims (apply min sh)
            diag-vals (for [i (range dims)] (mp/get-nd m (repeat rank i)))]
        (imp/construct m diag-vals))))

(extend-protocol mp/PSpecialisedConstructors
  #?(:clj Object :cljs object)
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
  #?(:clj Object :cljs object)
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
  #?(:clj Object :cljs object)
    (block-diagonal-matrix [m blocks]
      (let [aux (fn aux [acc blocks]
                  (if (empty? blocks)
                      acc
                      (let [acc-dim (long (mp/dimension-count acc 0))
                            new-block (blocks 0)
                            new-block-dim (long (mp/dimension-count new-block 0))
                            new-dim (+ acc-dim new-block-dim)
                            dm (vec (for [i (range new-dim)]
                                         (if (< i acc-dim)
                                             (into [] (concat (acc i)
                                                              (mp/new-vector [] new-block-dim)))
                                             (into [] (concat (mp/new-vector [] acc-dim)
                                                              (new-block (- i acc-dim)))))))]
                            (aux dm (subvec blocks 1)))))]
        (aux [] blocks))))

(extend-protocol mp/PMatrixPredicates
  #?(:clj Object :cljs object)
  (identity-matrix? [m]
    (let [rc (long (mp/dimension-count m 0))
          cc (long (mp/dimension-count m 1))]
      (if (and (== (long (mp/dimensionality m)) 2) (== rc cc))
        (loop [i (long 0)]
          (if (< i rc)
            (if (loop [j (long 0)]
                  (if (< j cc)
                    (let [elem (mp/get-2d m i j)]
                      (if (number? elem)
                        (if (== i j)
                          (if (== (double elem) 1.0) (recur (inc j)) false)
                          (if (zero? elem) (recur (inc j)) false))
                        false))
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
  #?(:clj Object :cljs object)
  (get-indices [a indices]
    (let [vals (map #(mp/get-nd a %1) (map mp/element-seq indices))] ;; TODO: use index coerce?
      (or
        (when (array? a) (mp/construct-matrix a vals))
        (mp/construct-matrix [] vals)))))

(extend-protocol mp/PIndicesSetting
  #?(:clj Object :cljs object)
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
  #?(:clj Object :cljs object)
  (non-zero-indices
    [m]
    (if (mp/is-vector? m)
      (vec (for [i (range (mp/dimension-count m 0))
                    :when (not (zero? (mp/get-1d m i)))]
              i))
      (vec (for [i (range (mp/dimension-count m 0))]
              (mp/non-zero-indices (mp/get-major-slice m i)))))))

;; TODO: proper generic implementations
(extend-protocol mp/PMatrixTypes
  #?(:clj Object :cljs object)
  (diagonal? [m]
    (if (= (long (mp/dimensionality m)) 2)
      (let [[^long mrows ^long mcols] (mp/get-shape m)]
        (->> (mp/element-seq m)
             (map (fn [^long i elem] (vector (quot i mcols) (rem i mcols) elem))
                  (range (* mrows mcols)))
             (every? (fn [[^long i ^long j v]]
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
           (mapcat (fn [[^long idx xs]] (drop (inc idx) xs)))
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
  nil
    (select [a area]
      (when (seq area) (error "Trying to select on nil with selection: " area))
      nil)
  #?(:clj Number :cljs number)
    (select [a area]
      (when (seq area) (error "Trying to select on numerical scalar with selection: " area))
      a)
  #?(:clj Object :cljs object)
    (select [a area]
      (or
        (mp/select-view a area) ;; use a view if supported by the implementation
        (wrap/wrap-selection a area))))

(extend-protocol mp/PSelectView
  nil
    (select-view [a area]
      (when (seq area) (error "Trying to select on nil with selection: " area))
      nil)  #?(:clj Object :cljs object)
  #?(:clj Number :cljs number)
    (select-view [a area]
      (when (seq area) (error "Trying to select on numerical scalar with selection: " area))
      a)
  #?(:clj Object :cljs object)
    (select-view [a area]
      (wrap/wrap-selection a area)))

(extend-protocol mp/PSelect
  #?(:clj Number :cljs number)
  (select [a area]
    (if (empty? area)
      a
      (error "Non empty area argument in select, called on Number " a))))

(defn- area-indices [area]
  (reduce (fn [io in]
            (for [a in b io]
              (cons a b))) (mapv vector (last area)) (rest (reverse area))))

(defn- indices [vals]
  (area-indices (mapv range (mp/get-shape vals))))


(extend-protocol mp/PSetSelection
  #?(:clj Object :cljs object)
  (set-selection [m area vals]
    (let [;; create a mutable clone
          mm (or (mp/mutable-matrix m)
                (construct-mutable-matrix m))
          v (mp/select-view m area)]
      (mp/assign! v vals)
      mm)))

(extend-protocol mp/PIndexImplementation
  #?(:clj Object :cljs object)
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

;; =======================================================
;; default label implementation

(extend-protocol mp/PDimensionLabels
  #?(:clj Object :cljs object)
    (label [m dim i]
      (if (<= 0 (long i) (dec (long (mp/dimension-count m dim))))
        nil
        (error "Dimension index out of range: " i)))
    (labels [m dim]
      (if (<= 0 (long dim) (dec (long (mp/dimensionality m))))
        nil
        (error "Dimension out of range: " dim))))

(extend-protocol mp/PColumnNames
  #?(:clj Object :cljs object)
    (column-name [m i]
      (let [dim (dec (long (mp/dimensionality m)))]
        (mp/label m dim i)))
    (column-names [m]
      (let [dim (dec (long (mp/dimensionality m)))]
        (mp/labels m dim))))


;; =======================================================
;; default linear algebra implementations

(extend-protocol mp/PNorm
  #?(:clj Object :cljs object)
  (norm [m p]
    (cond
      (= p #?(:clj Double/POSITIVE_INFINITY :cljs js/Number.POSITIVE_INFINITY)) (mp/element-max (mp/element-map m mops/abs))
      (number? p) (let [sum-of-element-powers (mp/element-sum (mp/element-pow (mp/element-map m mops/abs) p))]
                    (condp == p
                      1 sum-of-element-powers
                      2 (Math/sqrt sum-of-element-powers)
                      3 (Math/cbrt sum-of-element-powers)
                      (Math/pow sum-of-element-powers (/ 1.0 p))))
      :else (error "p must be a number"))))

;; QR decomposition utility functions

(defn compute-q [m ^doubles qr-data mcols mrows min-len
                 ^doubles us ^doubles vs ^doubles gammas]
  (let [q ^doubles (mp/to-double-array (mp/identity-matrix [] mrows))
        mcols (long mcols)
        mrows (long mrows)
        min-len (long min-len)]
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
                       (fn [^long i ^long j]
                         (aget q (+ (* i mrows) j))))))

(defn compute-r [m ^doubles data mcols mrows min-len compact?]
  (let [mrows (long mrows)
        mcols (long mcols)
        min-len (long min-len)
        cm (mp/compute-matrix
              m [mrows mcols]
              (fn [^long i ^long j]
                (if (and (< i min-len)
                         (>= j i)
                         (< j mcols))
                  (aget data (+ (* i mcols) j))
                  0)))]
    (if compact?
      (let [slcs (mp/get-major-slice-seq cm)
            non-zero-rows (long (reduce
                            (fn [^long cnt slice] (if (every? zero? slice) (inc cnt) cnt))
                            0
                            slcs))]
        ;; TODO: is this broken? Looks like mcols and mrows in wrong order?
        (mp/reshape cm [mcols (- mrows non-zero-rows)]))
      cm)))

(defn householder-qr [^doubles qr-data idx mcols
                      mrows ^doubles us ^doubles gammas]
  (let [idx (long idx)
        mcols (long mcols)
        mrows (long mrows)]
    (loop [qr-idx (long (+ idx (* idx mcols)))
           i (long idx)]
      (when (< i mrows)
        (aset us i (aget qr-data qr-idx))
        (recur (+ qr-idx mcols)
               (inc i))))
    (let [max_ (double (apply max (map #(Math/abs (double %))
                         (mp/subvector us idx (- mrows idx)))))]
      (if (= max_ 0.0)
        {:error true}
        (let [_ (c-for [i idx (< i mrows) (inc i)]
                  (aset us i (/ (aget us i) max_)))
              tau (->> (mp/subvector us idx (- mrows idx))
                       (reduce (fn [^double acc ^double x] (+ acc (* x x))) 0.0)
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
           :error false})))))

(defn update-qr [^doubles qr-data idx mcols mrows ^doubles vs
                 ^doubles us gamma tau]
  (let [idx (long idx)
        mrows (long mrows)
        mcols (long mcols)
        u (aget us idx)
        idx+1 (inc idx)
        gamma (double gamma)
        tau (double tau)]
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
  #?(:clj Object :cljs object)
  (qr [m options]
    (let [[mrows mcols] (mp/get-shape m)
          mrows (long mrows)
          mcols (long mcols)
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
(extend-protocol mp/PCholeskyDecomposition
  #?(:clj Object :cljs object)
  (cholesky [m options]
    (try-current-implementation m (mp/cholesky m options))))

(extend-protocol mp/PLUDecomposition
  #?(:clj Object :cljs object)
  (lu [m options]
    (try-current-implementation m (mp/lu m options))))

(extend-protocol mp/PSVDDecomposition
  #?(:clj Object :cljs object)
  (svd [m options]
    (try-current-implementation m (mp/svd m options))))

(extend-protocol mp/PEigenDecomposition
  #?(:clj Object :cljs object)
  (eigen [m options]
    (try-current-implementation m (mp/eigen m options))))

(extend-protocol mp/PSolveLinear
  #?(:clj Object :cljs object)
  (solve [a b]
    (try-current-implementation a (mp/solve a b))))

(extend-protocol mp/PLeastSquares
  #?(:clj Object :cljs object)
  (least-squares [a b]
    (try-current-implementation a (mp/least-squares a b))))

