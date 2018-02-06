(ns clojure.core.matrix.impl.persistent-vector
  "Namespace for core.matrix implementation using nested persistent vectors.

   Array format is defined as:
   - Top level object is an instance of clojure.lang.IPersistentVector
   - If the array is 1-dimensional each element is a scalar
   - Otherwise each element is an sub-array with identical shape (1 dimensional or more)

   Note that this allows for other array implementations to be nested inside persistent vectors,
   provided all nested arrays have the same shape and dimensionality of at least 1."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.common :refer [mapmatrix]]
            [clojure.core.matrix.impl.mathsops :as mops]
  #?@(:clj [[clojure.core.matrix.macros :refer [scalar-coerce error doseq-indexed]]
            [clojure.core.matrix.macros-clj :refer [native-array?]]]))

  #?(:clj (:import [clojure.lang IPersistentVector Indexed]
                   [java.util List])
      :cljs (:require-macros
              [clojure.core.matrix.impl.persistent-vector :refer [vector-1d?]]
              [clojure.core.matrix.macros :refer [scalar-coerce error doseq-indexed]]
              [clojure.core.matrix.macros-cljs :refer [native-array?]])))

#?(:clj (do
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* true)
))

;; (set! *unchecked-math* :warn-on-boxed) ;; use to check for boxing, some is unavoidable

;; =======================================================================
;; utility functions for manipulating persistent vector matrices
;;
;; Format assumed to be a nested vector
;;
;; Vectors can contain other matrices to add an extra dimension to another implementation.
;; this is a powerful feature - it means we can do higher dimensional work with matrices
;; even if the underlying implementation does not natively support this
;;
;; However: this also imposes limitations, in the sense that Persistent Vector matrices
;; cannot hold other array types without considering them to be part of the array structure.
;; This means that a 2D matrix of 1D vectors gets treated as a single 3D matrix. This may
;; cause some surprising / undefined behaviours.

(declare persistent-vector-coerce)

(defn coerce-nested
  "Ensures a vector is fully coerced to nested persistent vectors"
  ([v]
    (mapv persistent-vector-coerce v)))

#?(:clj
(defmacro vector-1d?
  "Utility macro to determine if a persistent vector represents a 1D vector"
  [pv]
  `(let [pv# ^IPersistentVector ~pv]
     (or (== 0 (count pv#)) (== 0 (long (mp/dimensionality (nth pv# 0)))))))
)

(defn- mapv-identity-check
  "Maps a function over a persistent vector, only modifying the vector if the function
   returns a different value"
  ([f ^#?(:clj IPersistentVector :cljs PersistentVector) v]
    (let [n  #?(:clj (.count v) :cljs (count v))]
      (loop [i 0 v v]
        (if (< i n)
          (let [x (nth v i)
                y (f x)]
            (recur (inc i) (if (identical? x y) v (assoc v i y))))
          v)))))

(defn- check-vector-shape
  ([v shape]
    (and
      (instance? #?(:clj IPersistentVector :cljs PersistentVector) v)
      (== (count v) (long (first shape)))
      (if-let [ns (next shape)]
        (every? #(check-vector-shape % ns) v)
        (every? #(not (instance? #?(:clj IPersistentVector :cljs PersistentVector) %)) v)))))

(defn is-nested-persistent-vectors?
  "Test if array is already in nested persistent vector array format."
  ([x]
    (cond
      (number? x) true
      (mp/is-scalar? x) true
      (not (instance? #?(:clj IPersistentVector :cljs PersistentVector) x)) false
      :else (and
              (every? is-nested-persistent-vectors? x)
              (check-vector-shape x (mp/get-shape x))))))

(defn persistent-vector-coerce
  "Coerces to nested persistent vectors"
  [x]
  (let [dims (long (mp/dimensionality x))]
    (cond
      (> dims 0) (mp/convert-to-nested-vectors x) ;; any array with 1 or more dimensions
      (and (== dims 0) (not (mp/is-scalar? x))) (mp/get-0d x) ;; array with zero dimensionality

      ;; it's not an array - so try alternative coercions
      (nil? x) x
      #?@(:clj [(.isArray (class x)) (mapv persistent-vector-coerce (seq x))])
      #?@(:clj [(instance? List x) (coerce-nested x)])
      (instance? #?(:clj Iterable :cljs IIterable) x) (coerce-nested x)
      (sequential? x) (coerce-nested x)

      ;; treat as a scalar value
      :default x)))

(defn vector-dimensionality
  "Calculates the dimensionality (== nesting depth) of nested persistent vectors"
  ^long [m]
  (cond
    (clojure.core/vector? m)
      (if (> (count m) 0)
        (+ 1 (vector-dimensionality (nth ^#?(:clj IPersistentVector :cljs PersistentVector) m 0)))
        1)
    :else (long (mp/dimensionality m))))

;; =======================================================================
;; Implementation for nested Clojure persistent vectors used as matrices


(extend-protocol mp/PImplementation
  #?(:clj IPersistentVector :cljs PersistentVector)
    (implementation-key [m] :persistent-vector)
    (meta-info [m]
      {:doc "Implementation for nested Clojure persistent vectors
             used as matrices"})
    (new-vector [m length] (vec (repeat length 0.0)))
    (new-matrix [m rows columns] (vec (repeat rows (mp/new-vector m columns))))
    (new-matrix-nd [m dims]
      (if-let [dims (seq dims)]
        (vec (repeat (first dims) (mp/new-matrix-nd m (next dims))))
        0.0))
    (construct-matrix [m data]
      (persistent-vector-coerce data))
    (supports-dimensionality? [m dims]
      true))

(extend-protocol mp/PBroadcast
  #?(:clj IPersistentVector :cljs PersistentVector)
    (broadcast [m target-shape]
      (let [mshape (mp/get-shape m)
            dims (long (count mshape))
            tdims (long (count target-shape))]
        (cond
          (> dims tdims)
            (error "Can't broadcast to a lower dimensional shape")
          (not (every? identity (map == mshape (take-last dims target-shape))))
            (error "Incompatible shapes, cannot broadcast shape " (vec mshape) " to " (vec target-shape))
          :else
            (reduce
              (fn [m dup] (vec (repeat dup m)))
              m
              (reverse (drop-last dims target-shape)))))))

(extend-protocol mp/PBroadcastLike
  #?(:clj IPersistentVector :cljs PersistentVector)
    (broadcast-like [m a]
      (mp/broadcast a (mp/get-shape m))))

(extend-protocol mp/PBroadcastCoerce
  #?(:clj IPersistentVector :cljs PersistentVector)
    (broadcast-coerce [m a]
      (mp/broadcast (persistent-vector-coerce a) (mp/get-shape m))))

(extend-protocol mp/PIndexedAccess
  #?(:clj IPersistentVector :cljs PersistentVector)
    (get-1d [m x]
      (let [r (nth m (int x))]
        (scalar-coerce r)))
    (get-2d [m x y]
      (let [row (nth m (int x))]
        (mp/get-1d row y)))
    (get-nd [m indexes]
      (if-let [indexes (seq indexes)]
        (if-let [next-indexes (next indexes)]
          (let [m (nth m (int (first indexes)))]
            (mp/get-nd m next-indexes))
          (nth m (int (first indexes))))
        m)))

;; we extend this so that nested mutable implementions are possible
(extend-protocol mp/PIndexedSetting
  #?(:clj IPersistentVector :cljs PersistentVector)
    (set-1d [m row v]
      (assoc m row v))
    (set-2d [m row column v]
      (assoc m row (mp/set-1d (m row) column v)))
    (set-nd [m indexes v]
      (if-let [indexes (seq indexes)]
        (let [fi (first indexes)]
          (if (== 1 (count indexes))
              (assoc m fi v)
              (assoc m fi (mp/set-nd (m fi) (next indexes) v))))
        (error "Trying to set on a persistent vector with insufficient indexes?")))
    (is-mutable? [m]
      ;; assume persistent vectors are immutable, even if they may have mutable components
      false))

(extend-protocol mp/PMatrixSlices
  #?(:clj IPersistentVector :cljs PersistentVector)
    (get-row [m i]
      (nth m (long i)))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (let [sl (nth m (long i))]
        sl))
    (get-slice [m dimension i]
      (let [dimension (long dimension)]
        (if (== dimension 0)
          (mp/get-major-slice m i)
          (let [sd (dec dimension)]
            (mapv #(mp/get-slice % sd i) m))))))

(extend-protocol mp/PMatrixRows
  #?(:clj IPersistentVector :cljs PersistentVector)
	  (get-rows [m]
      m))

(extend-protocol mp/PMatrixColumns
  #?(:clj IPersistentVector :cljs PersistentVector)
	  (get-columns [m]
      (vec (for [j (range (mp/dimension-count m 1))]
             (mapv #(mp/get-1d % j) m)))))

(extend-protocol mp/PSliceView
  #?(:clj IPersistentVector :cljs PersistentVector)
    (get-major-slice-view [m i] (nth m i)))

(extend-protocol mp/PSliceView2
  #?(:clj IPersistentVector :cljs PersistentVector)
    (get-slice-view [m dimension i]
      ;; delegate to get-slice
      (mp/get-slice m dimension i)))


(extend-protocol mp/PSliceSeq
  #?(:clj IPersistentVector :cljs PersistentVector)
    (get-major-slice-seq [m]
      m))

(extend-protocol mp/PSliceJoin
  #?(:clj IPersistentVector :cljs PersistentVector)
    (join [m a]
      (let [dims (long (mp/dimensionality m))
            adims (long (mp/dimensionality a))]
        (cond
          (== dims adims)
            (vec (concat (mp/get-major-slice-seq m) (mp/get-major-slice-seq a)))
          (== dims (inc adims))
            (conj m a)
          :else
            (error "Joining with array of incompatible size")))))

(extend-protocol mp/PRotate
  #?(:clj IPersistentVector :cljs PersistentVector)
    (rotate [m dim places]
      (let [dim (long dim)
            places (long places)]
        (if (== 0 dim)
         (let [c (long (count m))
               sh (long (if (> c 0) (mod places c) 0))]
           (if (== sh 0)
             m
             (vec (concat (subvec m sh c) (subvec m 0 sh)))))
         (mapv (fn [s] (mp/rotate s (dec dim) places)) m)))))

(extend-protocol mp/PTransposeDims
  #?(:clj IPersistentVector :cljs PersistentVector)
    (transpose-dims [m ordering]
      (if-let [ordering (seq ordering)]
        (let [dim (long (first ordering))
              next-ordering (map (fn [i] (if (< i dim) i (dec i))) (next ordering))
              slice-range (range (mp/dimension-count m dim))]
          (mapv (fn [si] (mp/transpose-dims (mp/get-slice m dim si) next-ordering)) slice-range))
        m)))

(extend-protocol mp/POrder
  #?(:clj IPersistentVector :cljs PersistentVector)
  (order
    ([m indices]
      (mapv #(nth m %) (mp/element-seq indices)))
    ([m dimension indices]
      (let [dimension (long dimension)]
        (if (== dimension 0)
          (mp/order m indices)
          (mapv #(mp/order % (dec dimension) indices) m))))))

(extend-protocol mp/PSubVector
  #?(:clj IPersistentVector :cljs PersistentVector)
    (subvector [m start length]
      (subvec m start (+ (long start) (long length)))))

(extend-protocol mp/PValidateShape
  #?(:clj IPersistentVector :cljs PersistentVector)
    (validate-shape 
      ([m]
        (if (mp/same-shapes? m)
          (mp/get-shape m)
          (error "Inconsistent shape for persistent vector array.")))
      ([m shape]
        (when (empty? shape) (error "Expected empty shape for persistent vector: " m)) 
        (if (apply = (next shape) (map mp/validate-shape m))
            shape
            (error "Inconsistent shape for persistent vector array, expected: " shape " on array " m)))))

(extend-protocol mp/PMatrixAdd
  #?(:clj IPersistentVector :cljs PersistentVector)
    (matrix-add [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix + m (persistent-vector-coerce a))))
    (matrix-sub [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix - m (persistent-vector-coerce a)))))

(extend-protocol mp/PVectorOps
  #?(:clj IPersistentVector :cljs PersistentVector)
    (vector-dot [a b]
      ;; optimised vector-dot for persistent vectors, handling 1D case
      (let [dims (long (mp/dimensionality b))]
        (cond
          (and (== dims 1) (== 1 (long (mp/dimensionality b))))
            (let [n (long (count a))]
              (cond
                (not= n (long (long (mp/dimension-count b 0)))) (error "Mismatched vector sizes")
                (instance? List b)
                  (let [b ^List b]
                    (loop [i 0 res 0.0]
                      (if (>= i n)
                        res
                        (recur (inc i) (+ res (* (double (nth a (int i))) (double (.get b (int i)))))))))
                (native-array? ^Object b)
                  (loop [i 0 res 0.0]
                    (if (>= i n)
                      res
                      (recur (inc i) (+ res (* (double (nth a (int i))) (double (nth b i)))))))
                :else
                  (reduce + (map * a (mp/element-seq b)))))

          :else (mp/inner-product a b))))
    (length [a]
      (if (number? (first a))
        (let [n (long (count a))]
         (loop [i 0 res 0.0]
           (if (< i n)
             (let [x (double (nth a i))]
               (recur (inc i) (+ res (* x x))))
             (Math/sqrt res))))
        (Math/sqrt (mp/length-squared a))))
    (length-squared [a]
      (if (number? (first a)) 
        (let [n (long (count a))]
          (loop [i 0 res 0.0]
            (if (< i n)
              (let [x (double (nth a i))]
                (recur (inc i) (+ res (* x x))))
              res)))
        (mp/element-reduce a (fn [^double r ^double x] (+ r (* x x))) 0.0)))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (mp/length-squared a))))))

(extend-protocol mp/PMutableMatrixConstruction
  #?(:clj IPersistentVector :cljs PersistentVector)
    (mutable-matrix [m]
      nil ;; fall-though: should get an ndarray result
      ))

(extend-protocol mp/PImmutableMatrixConstruction
  #?(:clj IPersistentVector :cljs PersistentVector)
  (immutable-matrix [m]
    m))

(extend-protocol mp/PVectorDistance
  #?(:clj IPersistentVector :cljs PersistentVector)
    (distance [a b] (mp/length (mp/matrix-sub b a))))

(extend-protocol mp/PSummable
  #?(:clj IPersistentVector :cljs PersistentVector)
    (element-sum [a]
      (mp/element-reduce a +)))

(extend-protocol mp/PCoercion
  #?(:clj IPersistentVector :cljs PersistentVector)
    (coerce-param [m param]
      (persistent-vector-coerce param)))

(extend-protocol mp/PMatrixEquality
  #?(:clj IPersistentVector :cljs PersistentVector)
    (matrix-equals [a b]
      (let [bdims (long (mp/dimensionality b))
            acount (long (count a))]
        (cond
          (<= bdims 0)
            false
          (not= acount (mp/dimension-count b 0))
            false
          (== 1 bdims)
            (and (== 1 (long (mp/dimensionality a)))
                 (loop [i 0]
                   (if (< i acount)
                     (if (mp/matrix-equals (nth a i) (mp/get-1d b i)) ;; can't avoid boxed warning, may be any sort of number
                       (recur (inc i))
                       false)
                     true)))
          (vector? b)
            (let [n acount]
               (loop [i 0]
                     (if (< i n)
                       (if (mp/matrix-equals (nth a i) (b i))
                         (recur (inc i))
                         false)
                       true)))
          :else
            (loop [sa (seq a) 
                   sb (mp/get-major-slice-seq b)]
              (if sa
                (if (mp/matrix-equals (first sa) (first sb))
                  (recur (next sa) (next sb))
                  false)
                true))))))

(extend-protocol mp/PMatrixMultiply
  #?(:clj IPersistentVector :cljs PersistentVector)
    (element-multiply [m a]
      (if (number? a)
        (mp/scale m a)
        (let [[m a] (mp/broadcast-compatible m a)]
          (mp/element-map m * a))))
    (matrix-multiply [m a]
      (let [mdims (long (mp/dimensionality m))
            adims (long (mp/dimensionality a))]
        (cond
          (== adims 0) (mp/scale m a)
          (and (== mdims 1) (== adims 2))
            (vec (for [i (range (mp/dimension-count a 1))]
                     (let [r (mp/get-column a i)]
                       (mp/vector-dot m r))))
          (and (== mdims 1) (== adims 1))
            (mp/vector-dot m a)
          (and (== mdims 2) (== adims 1))
            (mapv #(mp/vector-dot % a) m)
          (and (== mdims 2) (== adims 2))
            (mapv (fn [r]
                     (vec (for [j (range (mp/dimension-count a 1))]
                            (mp/vector-dot r (mp/get-column a j))))) m)
          :else
            (mp/inner-product m a)))))

(extend-protocol mp/PMatrixProducts
  #?(:clj IPersistentVector :cljs PersistentVector)
    (inner-product [m a]
      (let [adims (long (mp/dimensionality a))
            mdims (long (mp/dimensionality m))]
        (cond
          (== 0 adims)
            (mp/scale m (mp/get-0d a))
          (not (== (long (last (mp/get-shape m))) (long (first (mp/get-shape a)))))
            (error "Incompatible shapes for inner product: " (mp/get-shape m) " and " (mp/get-shape a))
          (== 1 mdims)
            (if (== 1 adims)
              (mp/element-sum (mp/element-multiply m a))
              (reduce mp/matrix-add (map (fn [sl x] (mp/scale sl x))
                                       (mp/get-major-slice-seq a)
                                       (mp/get-major-slice-seq m)))) ;; TODO: implement with mutable accumulation
          :else
           (mapv #(mp/inner-product % a) (mp/get-major-slice-seq m)))))
    (outer-product [m a]
      (mp/element-map m (fn [v] (mp/pre-scale a v)))))

(extend-protocol mp/PVectorTransform
  #?(:clj IPersistentVector :cljs PersistentVector)
    (vector-transform [m a]
      (mp/matrix-multiply m a))
    (vector-transform! [m a]
      (mp/assign! a (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixScaling
  #?(:clj IPersistentVector :cljs PersistentVector)
    (scale [m a]
      (mapmatrix #(* % a) m)) ;; can't avoid boxed warning, may be any sort of number
    (pre-scale [m a]
      (mapmatrix #(* a %) m))) ;; can't avoid boxed warning, may be any sort of number

(extend-protocol mp/PSquare
  #?(:clj IPersistentVector :cljs PersistentVector)
    (square [m]
      (mapmatrix * m m)))

(extend-protocol mp/PRowOperations
  #?(:clj IPersistentVector :cljs PersistentVector)
    (swap-rows [m i j]
      (let [i (long i)
            j (long j)]
        (if (== i j)
          m
          (assoc (assoc m i (m j)) j (m i)))))
    (multiply-row [m i factor]
      (assoc m i (mp/scale (m i) factor)))
    (add-row [m i j factor]
      (assoc m i (mp/matrix-add (m i) (mp/matrix-multiply (m j) factor)))))


;; helper functin to build generic maths operations
(defn build-maths-function
  ([[name func]]
    `(~name [~'m]
            (mapmatrix (fn [x#] (double (~func (double x#)))) ~'m))))

;; code generation for maths functions
;; we generate both name and name! versions
#?(:clj
(eval
  `(extend-protocol mp/PMathsFunctions
     #?(:clj IPersistentVector :cljs PersistentVector)
       ~@(map build-maths-function mops/maths-ops)
       ~@(map (fn [[name func]]
                (let [name (str name "!")
                      mname (symbol name)
                      mpmname (symbol "clojure.core.matrix.protocols" name)]
                  `(~mname [m#]
                     (doseq [s# (mp/get-major-slice-seq m#)]
                       (~mpmname s#)))))
              mops/maths-ops)))
)

(extend-protocol mp/PDimensionInfo
  #?(:clj IPersistentVector :cljs PersistentVector)
    (dimensionality [m]
      (if (== 0 (count m))
        1
        (inc (long (mp/dimensionality (nth m 0))))))
    (is-vector? [m]
      (vector-1d? m))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (let [c (count m)]
        (cons c (if (> c 0)
                  (mp/get-shape (#?(:clj .nth :cljs nth) m 0))
                  nil))))
    (dimension-count [m x]
      (let [x (long x)]
        (if (== x 0)
          #?(:clj (.length m)
             :cljs (count m))
          #?(:clj (mp/dimension-count (.nth m 0) (dec x))
             :cljs (mp/dimension-count (nth m 0) (dec x)))))))

(extend-protocol mp/PElementCount
  #?(:clj IPersistentVector :cljs PersistentVector)
    (element-count [m]
      (let [c (long (count m))]
        (if (== c 0)
          0
          (* c (mp/element-count #?(:clj (.nth m 0) :cljs (nth m 0)))))))) ;; can't avoid boxed warning here, may be a bigint

;; we need to implement this for all persistent vectors since we need to check all nested components
(extend-protocol mp/PConversion
  #?(:clj IPersistentVector :cljs PersistentVector)
    (convert-to-nested-vectors [m]
      (if (is-nested-persistent-vectors? m)
        m
        (let [m (mapv-identity-check mp/convert-to-nested-vectors m)
              m-shapes (map mp/get-shape m)]
          (if (every? (partial = (first m-shapes)) (rest m-shapes))
            m
            (error "Can't convert to persistent vector array: inconsistent shape."))))))

(defn- copy-to-double-array! 
  "Copy an arbitrary array to a region of a double array.
   Assumes size represents the element count of the array, must be greater than zero."
  ([m ^doubles arr ^long off ^long size]
    (cond
      ;; handle a single numerical value
      (number? m) (if (== size 1)
                    (aset arr off (double m))
                    (error "Invalid shape while copying to double array"))
      ;; handle a Clojure vector. Could have nested arrays
      (vector? m)
        (let [m ^IPersistentVector m
              ct (count m)
              skip (quot size ct)]
          (dotimes [i ct]
            (let [slc #?(:clj (.nth m i)
                         :cljs (nth m i))]
              (copy-to-double-array! slc arr (+ off (* i skip)) skip))))
      ;; otherwise, must be some arbitrary core.matrix array
      ;; TODO think of a faster way to implement this.
      :else
        (doseq-indexed [v (mp/element-seq m) i]
          (aset arr (+ off i) (double v))))))

(extend-protocol mp/PDoubleArrayOutput
  #?(:clj IPersistentVector :cljs PersistentVector)
    (to-double-array [m]
      (let [size (long (mp/element-count m))
            arr (double-array size)
            ct (count m)]
        (when (> size 0)
          (copy-to-double-array! m arr 0 size))
        arr))
    (as-double-array [m] nil))

(defn- copy-to-object-array [m ^objects arr ^long off ^long size]
  (let [ct (count m)]
    (cond
      ;; we need this to handle the case of non-vectors nested in vectors
      (not (vector? m))
        (doseq-indexed [v (mp/element-seq m) i]
          (aset arr (+ off i) v))
      ;; m must be a vector from now on
      (and (== size ct) (not (vector? (#?(:clj .nth :cljs nth)
                                       ^#?(:clj IPersistentVector :cljs PersistentVector) m 0 nil))))
        (dotimes [i size]
          (aset arr (+ off i) (nth ^#?(:clj IPersistentVector :cljs PersistentVector) m i)))
      :else
        (let [skip (quot size ct)]
          (dotimes [i ct]
            (copy-to-object-array (#?(:clj .nth :cljs nth)
                                   ^#?(:clj IPersistentVector :cljs PersistentVector) m i) arr (+ off (* i skip)) skip))))
    arr))

(extend-protocol mp/PObjectArrayOutput
  #?(:clj IPersistentVector :cljs PersistentVector)
    (to-object-array [m]
      (let [size (long (mp/element-count m))
            arr (object-array size)
            ct (count m)]
        (copy-to-object-array m arr 0 size)
        arr))
    (as-object-array [m] nil))

(extend-protocol mp/PFunctionalOperations
  #?(:clj IPersistentVector :cljs PersistentVector)
    (element-seq [m]
      (cond
        (== 0 (count m))
          nil
        (>= (long (mp/dimensionality (nth m 0))) 1)
          ;; we are a 2D+ array, so be conservative and create a concatenated sequence
          (mapcat mp/element-seq m)
        :else
          ;; we are a 1D vector, so already a valid seqable result for element-seq
          m))
    (element-map
      ([m f]
        (mapmatrix f m))
      ([m f a]
        (let [[m a] (mp/broadcast-same-shape m a)]
          (mapmatrix f m a)))
      ([m f a more]
        (let [arrays (cons m (cons a more))
              shapes (map mp/get-shape arrays)
              sh (or (mp/common-shape shapes) (error "Attempt to do element map with incompatible shapes: " (mapv mp/get-shape arrays)))
              arrays (map #(mp/broadcast % sh) arrays)]
          (apply mapmatrix f arrays))))
    (element-map!
      ([m f]
        (doseq [s m]
          (mp/element-map! s f))
        m)
      ([m f a]
        (dotimes [i (count m)]
          (mp/element-map! (m i) f (mp/get-major-slice a i)))
        m)
      ([m f a more]
        (dotimes [i (count m)]
          (apply mp/element-map! (m i) f (mp/get-major-slice a i) (map #(mp/get-major-slice % i) more)))
        m))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m)))))

(extend-protocol mp/PMapIndexed
  #?(:clj IPersistentVector :cljs PersistentVector)
    (element-map-indexed
      ([ms f]
       (let [dims (long (mp/dimensionality ms))]
         (cond
           (== 0 dims) (f [] (scalar-coerce ms))
           (== 1 dims) (vec (for [i (range (count ms))]
                              (f [i] (nth ms i))))
           :else       (vec (for [i (range (count ms))]
                              (mp/element-map-indexed (nth ms i) #(f (cons i %1) %2)))))))
      ([ms f as]
       (let [as   (mp/broadcast-like ms as)
             dima (long (mp/dimensionality as))]
         (if (mp/is-vector? ms)
           (do
             (when (> dima 1)
               (error "mapping with array of higher dimensionality?"))
             (when (and (== 1 dima)
                        (not= (mp/dimension-count ms 0) (mp/dimension-count as 0)))
               (error "Incompatible vector sizes"))
             (if (== 0 dima)
               (let [v (scalar-coerce as)]
                 (mapv #(f [%1] %2 v) (range (count ms))) ms)
               (mapv #(apply f [%1] %&) (range (count ms)) ms (mp/element-seq as))))
           (mapv (fn [i m a] (mp/element-map-indexed m #(apply f (cons i %1) %&) a))
                 (range (count ms)) ms (mp/get-major-slice-seq as)))))
      ([ms f as more]
       (if (mp/is-vector? ms)
         (apply mapv #(apply f [%1] %&) (range (count ms)) ms as more)
         (apply mapv (fn [i m a & mr]
                       (mp/element-map-indexed m #(apply f (cons i %1) %&) a mr))
                     (range (count ms)) ms as more))))
    (element-map-indexed!
      ([m f]
        (dotimes [i (count m)]
          (mp/element-map-indexed! (m i) #(f (cons i %1) %2)))
        m)
      ([m f a]
        (dotimes [i (count m)]
          (mp/element-map-indexed! (m i) #(apply f (cons i %1) %&)
                                   (mp/get-major-slice a i)))
        m)
      ([m f a more]
        (dotimes [i (count m)]
          (apply mp/element-map-indexed! (m i) #(apply f (cons i %1) %&)
                 (mp/get-major-slice a i) (map #(mp/get-major-slice % i) more)))
        m))
  )

(extend-protocol mp/PSelect
  #?(:clj IPersistentVector :cljs PersistentVector)
    (select
      ([a args]
       (if (= 1 (count args))
         (do
           (if (= 1 (mp/dimensionality a))
             (apply vector (mapv #(nth a %) (first args)))
             (error "Array dimension does not match length of args")))
         (apply vector (mapv #(mp/select (nth a %) (next args))
                             (first args)))))))

(extend-protocol mp/PIndexImplementation
  #?(:clj IPersistentVector :cljs PersistentVector)
	  (index? [m] true))

;; =====================================
;; Register implementation

(imp/register-implementation [1])
