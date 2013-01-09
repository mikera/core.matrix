(ns core.matrix.impl.persistent-vector
  (:require [core.matrix.protocols :as mp])
  (:use core.matrix)
  (:use core.matrix.utils)
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
    (if (number? (nth m 0))
      (mapv f m)
      (mapv (partial mapmatrix f) m)))
  ([f m1 m2]
    (if (number? (nth m1 0))
      (mapv f m1 m2)
      (mapv (partial mapmatrix f) m1 m2))))

;; =======================================================================
;; Implementation for standard Clojure persistent vectors used as matrices

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
        (mapv #(nth % i) m))))

(extend-protocol mp/PMatrixAdd
  clojure.lang.IPersistentVector
    (matrix-add [m a]
      (mapmatrix + m (coerce m a)))
    (matrix-sub [m a]
      (mapmatrix - m (coerce m a))))

(extend-protocol mp/PVectorOps
  clojure.lang.IPersistentVector
    (vector-dot [a b]
      (reduce + 0 (map (fn [x y] (* x y)) a (coerce a b))))
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
        (instance? java.util.List param) (coerce-nested param)
        (instance? java.lang.Iterable param) (coerce-nested param)
        :default (error "Can't coerce to vector: " (class param)))))

(extend-protocol mp/PMatrixMultiply
  clojure.lang.IPersistentVector
    (matrix-multiply [m a]
      (if (vector? a)
        (error "not yet implemented")
        (mm/mul m a)))
    (scale [m a]
      (let [a (double a)]
        (mapmatrix (partial * a) m))))

;; helper functin to build generic maths operations
(defn build-maths-function 
  ([[name func]]
    `(~name [~'m]
            (mapmatrix (fn [x#] (double (~func (double x#)))) ~'m))))

(eval
  `(extend-protocol mp/PMathsFunctions
     clojure.lang.IPersistentVector
       ~@(map build-maths-function mops/maths-ops)))

(extend-protocol mp/PDimensionInfo
  clojure.lang.IPersistentVector
    (dimensionality [m]
      (let [fst (.nth m (int 0))]
        (inc (mp/dimensionality fst))))
    (is-vector? [m]
      (== 1 (mp/dimensionality m)))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (mp/dimension-count (m 0) (dec x)))))

