(ns core.matrix.impl.persistent-vector
  (:require [core.matrix.protocols :as mp])
  (:use core.matrix)
  (:use core.matrix.utils)
  (:require [core.matrix.implementations :as imp])
  (:require [core.matrix.impl.mathsops :as mops])
  (:require [core.matrix.multimethods :as mm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =======================================================================
;; utility functions for manipulating persistent vector matrices
;;
;; format assumed to be a nested vector of Numbers

(defn coerce-nested
  "Ensures a vector is fully coerced to nested persistent vectors"
  ([v]
    (mapv #(if (number? %) % (coerce-nested %)) v)))

(defn mapmatrix
  "Maps a function over all components of a persistent vector matrix. Like mapv but for matrices"
  ([f m]
    (if (mp/is-scalar? (nth m 0))
      (mapv f m)
      (mapv (partial mapmatrix f) m)))
  ([f m1 m2]
    (if (number? (nth m1 0))
      (mapv f m1 m2)
      (mapv (partial mapmatrix f) m1 m2)))
  ([f m1 m2 & more]
    (if (number? (nth m1 0))
      (apply mapv f m1 m2 more)
      (apply mapv (partial mapmatrix f) m1 m2 more))))

;; =======================================================================
;; Implementation for nested Clojure persistent vectors used as matrices


(extend-protocol mp/PImplementation
  clojure.lang.IPersistentVector
    (implementation-key [m] :persistent-vector)
    (new-vector [m length] (vec (repeat length 0.0)))
    (new-matrix [m rows columns] (vec (repeat rows (mp/new-vector m columns))))
    (new-matrix-nd [m dims] 
      (if-let [dims (seq dims)] 
        (vec (repeat (first dims) (mp/new-matrix-nd m (next dims))))
        0.0))
    (construct-matrix [m data]
      (cond 
        (mp/is-scalar? data) 
          data
        (>= (dimensionality data) 1) 
          (mapv #(mp/construct-matrix m %) (slices data))
        (sequential? data)
          (mapv #(mp/construct-matrix m %) data)
        :default
          (error "Don't know how to construct matrix from: " (class data))))
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

(extend-protocol mp/PMatrixSlices
  clojure.lang.IPersistentVector
    (get-row [m i]
      (.nth m (long i)))
    (get-column [m i]
      (let [i (long i)]
        (mapv #(nth % i) m)))
    (get-major-slice [m i]
      (m i))
    (get-slice [m dimension i]
      (let [i (long i)
            dimension (long dimension)]
        (if (== dimension 0)
          (mp/get-major-slice m i)
          (mapv #(mp/get-slice % (dec dimension) i) m)))))

(extend-protocol mp/PMatrixAdd
  clojure.lang.IPersistentVector
    (matrix-add [m a]
      (mapmatrix + m (coerce m a)))
    (matrix-sub [m a]
      (mapmatrix - m (coerce m a))))

(extend-protocol mp/PVectorOps
  clojure.lang.IPersistentVector
    (vector-dot [a b]
      (reduce + 0 (map * a (coerce a b))))
    (length-squared [a]
      (reduce + (map #(* % %) a)))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (length-squared a))))))

(extend-protocol mp/PCoercion
  clojure.lang.IPersistentVector
    (coerce-param [m param]
      (cond
        (clojure.core/vector? param) param
        (number? param) param
        (sequential? param) (coerce-nested param)
        (scalar? param) param
        (instance? java.util.List param) (coerce-nested param)
        (instance? java.lang.Iterable param) (coerce-nested param)
        :default (error "Can't coerce to vector: " (class param)))))

(extend-protocol mp/PMatrixMultiply
  clojure.lang.IPersistentVector
    (element-multiply [m a]
      (emap * m a))
    (matrix-multiply [m a]
      (cond 
        (and (matrix-2d? m) (mp/is-vector? a))
	        (let [[rows cols] (shape m)]
	          (vec (for [i (range rows)]
	                 (let [r (get-row m i)]
	                   (dot r a)))))
        (and (matrix-2d? m) (matrix-2d? m))
          (let [[rows cols] (shape m)]
            (vec (for [i (range rows)]
                   (let [r (get-row m i)]
                     (vec (for [j (range cols)]
                            (dot r (get-column a j))))))))
        :default
          (mm/mul m a))))

(extend-protocol mp/PVectorTransform
  clojure.lang.PersistentVector
    (vector-transform [m a]
      (mul m a))
    (vector-transform! [m a]
      (assign! a (mul m a))))

(extend-protocol mp/PMatrixScaling
  clojure.lang.IPersistentVector
    (scale [m a]
      (let [a (double a)]
        (mapmatrix #(* % a) m)))
    (pre-scale [m a]
      (let [a (double a)]
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
                `(~(symbol (str name "!")) [~'m]
                   (error "Persistent vector matrices are not mutable!"))) mops/maths-ops)))

(extend-protocol mp/PDimensionInfo
  clojure.lang.IPersistentVector
    (dimensionality [m]
      (let [fst (.nth m (int 0))]
        (inc (mp/dimensionality fst))))
    (is-vector? [m]
      (== 1 (mp/dimensionality m)))
    (is-scalar? [m]
      false)
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (mp/dimension-count (m 0) (dec x)))))

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
        (error "Persistent vector matrices are not mutable!"))
      ([m f a]
        (error "Persistent vector matrices are not mutable!"))
      ([m f a more]
        (error "Persistent vector matrices are not mutable!")))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m)))))

;; =====================================
;; Register implementation

(imp/register-implementation [])
