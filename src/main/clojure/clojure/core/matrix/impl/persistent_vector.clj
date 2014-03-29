(ns clojure.core.matrix.impl.persistent-vector
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:import clojure.lang.IPersistentVector))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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

(defmacro vector-1d? [pv]
  `(let [^clojure.lang.IPersistentVector pv# ~pv]
     (or (== 0 (.length pv#)) (== 0 (mp/dimensionality (.nth pv# 0))))))

(defn- mapmatrix
  "Maps a function over all components of a persistent vector matrix. Like mapv but for matrices.
   Assumes correct dimensionality / shape.

   Returns a nested persistent vector matrix or a scalar value."
  ([f m]
    (let [dims (long (mp/dimensionality m))]
      (cond
        (== 0 dims) (f (scalar-coerce m))
        (== 1 dims) (mapv #(f (scalar-coerce %)) m)
        :else (mapv (partial mapmatrix f) m))))
  ([f m1 m2]
    (let [dim2 (long (mp/dimensionality m2))]
      (cond (mp/is-vector? m1)
        (do
          (when (> dim2 1) (error "mapping with array of higher dimensionality?"))
          (when (and (== 1 dim2) (not= (mp/dimension-count m1 0) (mp/dimension-count m2 0))) (error "Incompatible vector sizes"))
          (if (== 0 dim2)
            (let [v (scalar-coerce m2)] (mapv #(f % v) m1 ))
            (mapv f m1 (mp/element-seq m2))))
        :else
          (mapv (partial mapmatrix f) 
                m1 
                (mp/get-major-slice-seq m2)))))
  ([f m1 m2 & more]
    (if (mp/is-vector? m1)
      (apply mapv f m1 m2 more)
      (apply mapv (partial mapmatrix f) m1 m2 more))))

(defn- mapv-identity-check
  "Maps a function over a persistent vector, only modifying the vector if the function
   returns a different value"
  ([f ^clojure.lang.IPersistentVector v]
    (let [n (.count v)]
      (loop [i 0 v v]
        (if (< i n)
          (let [x (.nth v i)
                y (f x)]
            (recur (inc i) (if (identical? x y) v (assoc v i y))))
          v)))))

(defn- check-vector-shape
  ([v shape]
    (and
      (instance? IPersistentVector v)
      (== (count v) (first shape))
      (if-let [ns (next shape)]
        (every? #(check-vector-shape % ns) v)
        (every? #(not (instance? IPersistentVector %)) v)))))

(defn is-nested-persistent-vectors? 
  "Test if array is already in nested persistent vector array format."
  ([x]
    (cond
      (number? x) true
      (mp/is-scalar? x) true
      (not (instance? clojure.lang.IPersistentVector x)) false
      :else (and 
              (every? is-nested-persistent-vectors? x)
              (check-vector-shape x (mp/get-shape x))))))

(defn persistent-vector-coerce [x]
  "Coerces to nested persistent vectors"
  (let [dims (long (mp/dimensionality x))]
    (cond
      (> dims 0) (mp/convert-to-nested-vectors x) ;; any array with 1 or more dimensions
      (and (== dims 0) (not (mp/is-scalar? x))) (mp/get-0d x) ;; array with zero dimensionality
      
      ;; it's not an array - so try alternative coercions
      (nil? x) x
      (.isArray (class x)) (map persistent-vector-coerce (seq x))
      (instance? java.util.List x) (coerce-nested x)
      (instance? java.lang.Iterable x) (coerce-nested x)
      (sequential? x) (coerce-nested x)
      
      ;; treat as a scalar value
      :default x)))

(defn vector-dimensionality [m]
  "Calculates the dimensionality (== nesting depth) of nested persistent vectors"
  (cond
    (clojure.core/vector? m)
      (if (> (count m) 0)
        (+ 1 (vector-dimensionality (.nth ^IPersistentVector m 0)))
        1)
    :else (mp/dimensionality m)))

;; =======================================================================
;; Implementation for nested Clojure persistent vectors used as matrices


(extend-protocol mp/PImplementation
  IPersistentVector
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
  IPersistentVector
    (broadcast [m target-shape]
      (let [mshape (mp/get-shape m)
            dims (long (count mshape))
            tdims (long (count target-shape))]
        (cond
          (> dims tdims) 
            (error "Can't broadcast to a lower dimensional shape")
          (not (every? identity (map #(== %1 %2) mshape (take-last dims target-shape))))
            (error "Incompatible shapes, cannot broadcast " (vec mshape) " to " (vec target-shape))
          :else
            (reduce
              (fn [m dup] (vec (repeat dup m)))
              m
              (reverse (drop-last dims target-shape)))))))

(extend-protocol mp/PBroadcastLike
  IPersistentVector
    (broadcast-like [m a]
      (mp/broadcast a (mp/get-shape m))))

(extend-protocol mp/PBroadcastCoerce
  IPersistentVector
    (broadcast-coerce [m a]
      (mp/broadcast (persistent-vector-coerce a) (mp/get-shape m))))

(extend-protocol mp/PIndexedAccess
  IPersistentVector
    (get-1d [m x]
      (let [r (.nth m (int x))]
        (scalar-coerce r)))
    (get-2d [m x y]
      (let [row (.nth m (int x))]
        (mp/get-1d row y)))
    (get-nd [m indexes]
      (if-let [indexes (seq indexes)]
        (if-let [next-indexes (next indexes)]
          (let [m (.nth m (int (first indexes)))]
            (mp/get-nd m next-indexes))
          (.nth m (int (first indexes))))
        m)))

;; we extend this so that nested mutable implementions are possible
(extend-protocol mp/PIndexedSetting
  IPersistentVector
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
      false))

(extend-protocol mp/PMatrixSlices
  IPersistentVector
    (get-row [m i]
      (.nth m (long i)))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (let [sl (.nth m (long i))]
        sl))
    (get-slice [m dimension i]
      (let [dimension (long dimension)]
        (if (== dimension 0)
          (mp/get-major-slice m i)
          (let [sd (dec dimension)]
            (mapv #(mp/get-slice % sd i) m))))))

(extend-protocol mp/PSliceView
  IPersistentVector
    (get-major-slice-view [m i] (.nth m i)))

(extend-protocol mp/PSliceSeq
  IPersistentVector
    (get-major-slice-seq [m]
      (if (vector-1d? m) 
        (seq (map mp/get-0d m))
        (seq m))))

(extend-protocol mp/PSliceJoin
  IPersistentVector
    (join [m a]
      (let [dims (mp/dimensionality m)
            adims (mp/dimensionality a)]
        (cond
          (== dims adims)
            (vec (concat (mp/get-major-slice-seq m) (mp/get-major-slice-seq a)))
          (== dims (inc adims))
            (conj m a)
          :else
            (error "Joining with array of incompatible size")))))

(extend-protocol mp/PRotate
  IPersistentVector
    (rotate [m dim places]
      (if (== 0 dim)
        (let [c (count m)
              sh (if (> c 0) (mod places c) 0)]
          (if (== sh 0)
            m
            (vec (concat (subvec m sh c) (subvec m 0 sh)))))
        (mapv (fn [s] (mp/rotate s (dec dim) places)) m))))

(extend-protocol mp/POrder
  IPersistentVector
  (order
    ([m cols]
       (mp/order m 0 cols))
    ([m dimension cols]
       (mapmatrix #(mp/get-slice m dimension %) cols))))

(extend-protocol mp/PSubVector
  IPersistentVector
    (subvector [m start length]
      (subvec m start (+ start length))))

(extend-protocol mp/PValidateShape
  IPersistentVector
    (validate-shape [m]
      (if (mp/same-shapes? m)
        (mp/get-shape m)
        (error "Inconsistent shape for persistent vector array.")))) 

(extend-protocol mp/PMatrixAdd
  IPersistentVector
    (matrix-add [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix + m (persistent-vector-coerce a))))
    (matrix-sub [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix - m (persistent-vector-coerce a)))))

(extend-protocol mp/PVectorOps
  IPersistentVector
    (vector-dot [a b]
      (let [dims (long (mp/dimensionality b))
            ;; b (persistent-vector-coerce b)
            ]
        (cond
          (and (== dims 1) (instance? clojure.lang.Indexed b))
            (do 
              (when-not (== (count a) (count b)) (error "Mismatched vector sizes"))
              (reduce + 0 (map * a b)))
          (== dims 0) (mp/scale a b)
          :else (mp/inner-product a b))))
    (length [a]
      (Math/sqrt (double (reduce + (map #(* % %) a)))))
    (length-squared [a]
      (reduce + (map #(* % %) a)))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (mp/length-squared a))))))

(extend-protocol mp/PMutableMatrixConstruction
  IPersistentVector
    (mutable-matrix [m]
      nil ;; fall-though: should get an ndarray result
      ))

(extend-protocol mp/PVectorDistance
  IPersistentVector
    (distance [a b] (mp/length (mapv - b a))))

(extend-protocol mp/PSummable
  IPersistentVector
    (element-sum [a]
      (mp/element-reduce a +)))

(extend-protocol mp/PCoercion
  IPersistentVector
    (coerce-param [m param]
      (persistent-vector-coerce param)))

(extend-protocol mp/PMatrixEquality
  IPersistentVector
    (matrix-equals [a b]
      (let [bdims (long (mp/dimensionality b))]
        (cond
          (<= bdims 0) 
            false
          (not= (count a) (mp/dimension-count b 0)) 
            false
          (== 1 bdims)
            (and (== 1 (mp/dimensionality a))
                 (let [n (long (count a))]
                   (loop [i 0]
                     (if (< i n)
                       (if (== (mp/get-1d a i) (mp/get-1d b i)) 
                         (recur (inc i))
                         false)
                       true))))
          (vector? b)
            (let [n (long (count a))]
               (loop [i 0]
                     (if (< i n)
                       (if (mp/matrix-equals (a i) (b i)) 
                         (recur (inc i))
                         false)
                       true)))
          :else 
            (loop [sa (seq a) sb (mp/get-major-slice-seq b)]
              (if sa
                (if (mp/matrix-equals (first sa) (first sb))
                  (recur (next sa) (next sb))
                  false)
                true))))))

(extend-protocol mp/PMatrixMultiply
  IPersistentVector
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
          (and (== mdims 2) (== adims 1))
            (mapv #(mp/vector-dot % a) m)
          (and (== mdims 2) (== adims 2))
            (mapv (fn [r]
                     (vec (for [j (range (mp/dimension-count a 1))]
                            (mp/vector-dot r (mp/get-column a j))))) m)
          :else
            (mm/mul m a)))))

(extend-protocol mp/PVectorTransform
  IPersistentVector
    (vector-transform [m a]
      (mp/matrix-multiply m a))
    (vector-transform! [m a]
      (mp/assign! a (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixScaling
  IPersistentVector
    (scale [m a]
      (let [a (mp/get-0d a)]
        (mapmatrix #(* % a) m)))
    (pre-scale [m a]
      (let [a (mp/get-0d a)]
        (mapmatrix #(* a %) m))))

(extend-protocol mp/PSquare
  IPersistentVector
    (square [m] 
      (mapmatrix #(* % %) m)))

(extend-protocol mp/PRowOperations
  IPersistentVector
    (swap-rows [m i j]
      (if (== i j)
        m
        (assoc (assoc m i (m j)) j (m i))))
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
(eval
  `(extend-protocol mp/PMathsFunctions
     clojure.lang.IPersistentVector
       ~@(map build-maths-function mops/maths-ops)
       ~@(map (fn [[name func]]
                (let [name (str name "!")
                      mname (symbol name)
                      mpmname (symbol "clojure.core.matrix.protocols" name)]
                  `(~mname [m#]
                     (doseq [s# (mp/get-major-slice-seq m#)]
                       (~mpmname s#)))))
              mops/maths-ops)))

(extend-protocol mp/PDimensionInfo
  IPersistentVector
    (dimensionality [m]
      (if (== 0 (.length m))
        1
        (inc (mp/dimensionality (.nth m 0)))))
    (is-vector? [m]
      (vector-1d? m))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (let [c (.length m)]
        (cons c (if (> c 0)
                  (mp/get-shape (m 0))
                  nil))))
    (dimension-count [m x]
      (if (== x 0)
        (.length m)
        (mp/dimension-count (m 0) (dec x)))))

(extend-protocol mp/PElementCount
  IPersistentVector
    (element-count [m] 
      (let [c (long (count m))] 
        (if (== c 0) 
          0
          (* c (mp/element-count (m 0)))))))

;; we need to implement this for all persistent vectors since we need to check all nested components
(extend-protocol mp/PConversion
  IPersistentVector
    (convert-to-nested-vectors [m]
      (if (is-nested-persistent-vectors? m)
        m
        (let [m (mapv-identity-check mp/convert-to-nested-vectors m)
              m-shapes (map mp/get-shape m)]
          (if (every? (partial = (first m-shapes)) (rest m-shapes))
            m
            (error "Can't convert to persistent vector array: inconsistent shape."))))))

(extend-protocol mp/PFunctionalOperations
  IPersistentVector
    (element-seq [m]
      (cond
        (== 0 (count m)) 
          '()
        (> (mp/dimensionality (m 0)) 0)
          (mapcat mp/element-seq m)
        :else
          (map mp/get-0d m)))
    (element-map
      ([m f]
        (mapmatrix f m))
      ([m f a]
        (mapmatrix f m (mp/broadcast-like m a)))
      ([m f a more]
        (apply mapmatrix f m a more)))
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

;; =====================================
;; Register implementation

(imp/register-implementation [1])
