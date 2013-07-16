(ns clojure.core.matrix.impl.persistent-vector
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm]))

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

(declare persistent-vector-coerce)

(defn coerce-nested
  "Ensures a vector is fully coerced to nested persistent vectors"
  ([v]
    (mapv persistent-vector-coerce v)))

(defn vector-1d? [^clojure.lang.IPersistentVector pv]
  (or (== 0 (.length pv)) (mp/is-scalar? (.nth pv 0))))

(defn- mapmatrix
  "Maps a function over all components of a persistent vector matrix. Like mapv but for matrices.
   Assumes correct dimensionality / shape.

   Returns a nested persistent vector matrix or a scalar value."
  ([f m]
    (let [dims (long (mp/dimensionality m))]
      (cond
        (== 0 dims) (f (mp/get-0d m))
        (== 1 dims) (mapv f (mp/element-seq m))
        :else (mapv (partial mapmatrix f) m))))
  ([f m1 m2]
    (if (mp/is-vector? m1)
      (let [dim2 (long (mp/dimensionality m2))]
        (when (> dim2 1) (error "mapping with array of higher dimensionality?"))
        (when (and (== 1 dim2) (not= (mp/dimension-count m1 0) (mp/dimension-count m2 0))) (error "Incompatible vector sizes"))
        (if (== 0 dim2)
          (let [v (mp/get-0d m2)] (mapv #(f % v) m1 ))
          (mapv f m1 (mp/element-seq m2))))
      (mapv (partial mapmatrix f) m1 (mp/get-major-slice-seq  m2))))
  ([f m1 m2 & more]
    (if (mp/is-vector? m1)
      (apply mapv f m1 m2 more)
      (apply mapv (partial mapmatrix f) m1 m2 more))))

(defn is-nested-persistent-vectors? [x]
  (cond
    (number? x) true
    (mp/is-scalar? x) true
    (not (instance? clojure.lang.IPersistentVector x)) false
    :else (every? is-nested-persistent-vectors? x)))

(defn persistent-vector-coerce [x]
  "Coerces to nested persistent vectors"
  (let [dims (mp/dimensionality x)]
    (cond
        (and (== dims 0) (not (mp/is-scalar? x))) (mp/get-0d x) ;; arrays with zero dimensionality
      (> dims 0) (mp/convert-to-nested-vectors x)
        (clojure.core/vector? x)
        (if (is-nested-persistent-vectors? x) x (mapv mp/convert-to-nested-vectors x))
        (nil? x) x
      (.isArray (class x)) (map persistent-vector-coerce (seq x))
        (instance? java.util.List x) (coerce-nested x)
        (instance? java.lang.Iterable x) (coerce-nested x)
        (sequential? x) (coerce-nested x)
      (mp/is-scalar? x) x
        :default (error "Can't coerce to vector: " (class x)))))

(defn vector-dimensionality [m]
  "Calculates the dimensionality (== nesting depth) of nested persistent vectors"
  (cond
    (clojure.core/vector? m)
      (if (> (count m) 0)
        (+ 1 (vector-dimensionality (m 0)))
        1)
    (mp/is-scalar? m) 0
    :else (long (mp/dimensionality m))))

(defn is-nested-vectors?
  "Returns true if m is in a correct nested vector implementation."
  ([m]
    (or (mp/is-scalar? m)
        (and (clojure.core/vector? m)
             (every? is-nested-vectors? m)))))

;(defmacro with-broadcasting [syms form]
;  (let [shape-syms (map (fn [_] (gensym "shape")) syms)]
;    `(let [~(interleave shape-syms (map (fn [s] `(mp/get-shape ~s)) syms))
;           bs# (broadcast-shape ~shape-syms)])))
;; TODO comp[lete broadcasting macro

;; =======================================================================
;; Implementation for nested Clojure persistent vectors used as matrices


(extend-protocol mp/PImplementation
  clojure.lang.IPersistentVector
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

(extend-protocol mp/PIndexedAccess
  clojure.lang.IPersistentVector
    (get-1d [m x]
      (.nth m (int x)))
    (get-2d [m x y]
      (let [row (.nth m (int x))]
        (mp/get-1d row y)))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.nth m (int (first indexes)))]
          (mp/get-nd m next-indexes))
        (.nth m (int (first indexes))))))

;; we extend this so that nested mutable implementions are possible
(extend-protocol mp/PIndexedSetting
  clojure.lang.IPersistentVector
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
      (if (vector-1d? m)
        false
        (mp/is-mutable? (m 0)))))

(extend-protocol mp/PMatrixSlices
  clojure.lang.IPersistentVector
    (get-row [m i]
      (.nth m (long i)))
    (get-column [m i]
      (mp/get-slice m 1 i))
    (get-major-slice [m i]
      (let [sl (m i)]
        sl))
    (get-slice [m dimension i]
      (let [i (long i)
            dimension (long dimension)]
        (if (== dimension 0)
          (mp/get-major-slice m i)
          (let [sd (dec dimension)]
            (mapv #(mp/get-slice % sd i) m))))))

(extend-protocol mp/PSliceView
  clojure.lang.IPersistentVector
    (get-major-slice-view [m i] (m i)))

(extend-protocol mp/PSliceSeq
  clojure.lang.IPersistentVector
    (get-major-slice-seq [m]
      (seq m)))

(extend-protocol mp/PSliceJoin
  clojure.lang.IPersistentVector
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

(extend-protocol mp/PSubVector
  clojure.lang.IPersistentVector
    (subvector [m start length]
      (subvec m start (+ start length))))

(extend-protocol mp/PMatrixAdd
  clojure.lang.IPersistentVector
    (matrix-add [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix + m (persistent-vector-coerce a))))
    (matrix-sub [m a]
      (let [[m a] (mp/broadcast-compatible m a)]
        (mapmatrix - m (persistent-vector-coerce a)))))

(extend-protocol mp/PVectorOps
  clojure.lang.IPersistentVector
    (vector-dot [a b]
      (let [b (persistent-vector-coerce b)]
        (when-not (== (count a) (count b)) (error "Mismatched vector sizes"))
        (reduce + 0 (map * a b))))
    (length [a]
      (Math/sqrt (double (reduce + (map #(* % %) a)))))
    (length-squared [a]
      (reduce + (map #(* % %) a)))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (mp/length-squared a))))))

(extend-protocol mp/PMutableMatrixConstruction
  clojure.lang.IPersistentVector
    (mutable-matrix [m]
      nil ;; fall-though: should get an ndarray result
      ))

(extend-protocol mp/PVectorDistance
  clojure.lang.IPersistentVector
    (distance [a b] (mp/length (mapv - b a))))

(extend-protocol mp/PSummable
  clojure.lang.IPersistentVector
    (element-sum [a]
      (mp/element-reduce a +)))

(extend-protocol mp/PCoercion
  clojure.lang.IPersistentVector
    (coerce-param [m param]
      (persistent-vector-coerce param)))

(extend-protocol mp/PMatrixMultiply
  clojure.lang.IPersistentVector
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
  clojure.lang.PersistentVector
    (vector-transform [m a]
      (mp/matrix-multiply m a))
    (vector-transform! [m a]
      (mp/assign! a (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixScaling
  clojure.lang.IPersistentVector
    (scale [m a]
      (let [a (mp/get-0d a)]
        (mapmatrix #(* % a) m)))
    (pre-scale [m a]
      (let [a (mp/get-0d a)]
        (mapmatrix (partial * a) m))))

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
  clojure.lang.IPersistentVector
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

;; we need to implement this for all persistent vectors since we need to check all nested components
(extend-protocol mp/PConversion
  clojure.lang.IPersistentVector
    (convert-to-nested-vectors [m]
      (mapv mp/convert-to-nested-vectors m)))

(extend-protocol mp/PFunctionalOperations
  clojure.lang.IPersistentVector
    (element-seq [m]
      (mapcat mp/element-seq m))
    (element-map
      ([m f]
        (mapmatrix f m))
      ([m f a]
        (mapmatrix f m a))
      ([m f a more]
        (apply mapmatrix f m a more)))
    (element-map!
      ([m f]
        (doseq [s m] (mp/element-map! s f))
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
