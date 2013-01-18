(ns core.matrix.impl.persistent-vector
  (:require [core.matrix.protocols :as mp])
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

(defn persistent-vector-coerce [x]
  "Coerces to persistent vectors"
  (cond
    (clojure.core/vector? x) x
    (number? x) x
    (sequential? x) (coerce-nested x)
    (mp/is-scalar? x) x
    (instance? java.util.List x) (coerce-nested x)
    (instance? java.lang.Iterable x) (coerce-nested x)
    :default (error "Can't coerce to vector: " (class x)))) 

(defn vector-dimensionality ^long [m]
  "Calculates the dimensionality (== nesting depth) of nested persistent vectors"
  (cond
    (clojure.core/vector? m) 
      (if (> (count m) 0) 
        (+ 1 (vector-dimensionality (m 0))) 
        1)
    (mp/is-scalar? m) 0
    :else (mp/dimensionality m))) 

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
        (>= (mp/dimensionality data) 1) 
          (mapv #(mp/construct-matrix m %) (for [i (range (mp/dimension-count data 0))] (mp/get-major-slice data i)))
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
      (mapmatrix + m (persistent-vector-coerce a)))
    (matrix-sub [m a]
      (mapmatrix - m (persistent-vector-coerce a))))

(extend-protocol mp/PVectorOps
  clojure.lang.IPersistentVector
    (vector-dot [a b]
      (reduce + 0 (map * a (persistent-vector-coerce b))))
    (length-squared [a]
      (reduce + (map #(* % %) a)))
    (normalise [a]
      (mp/scale a (/ 1.0 (Math/sqrt (mp/length-squared a))))))

(extend-protocol mp/PCoercion
  clojure.lang.IPersistentVector
    (coerce-param [m param]
      (persistent-vector-coerce param)))

(extend-protocol mp/PMatrixMultiply
  clojure.lang.IPersistentVector
    (element-multiply [m a]
      (mp/element-map m * a))
    (matrix-multiply [m a]
      (cond 
        (and (mp/is-vector? a)  )
	        (let [[rows cols] (mp/get-shape m)]
	          (vec (for [i (range rows)]
	                 (let [r (m i)]
	                   (mp/vector-dot r a)))))
        (and (== 2 (vector-dimensionality m)) (== 2 (mp/dimensionality m)))
          (let [[rows cols] (mp/get-shape m)]
            (vec (for [i (range rows)]
                   (let [r (m i)]
                     (vec (for [j (range cols)]
                            (mp/vector-dot r (mp/get-column a j))))))))
        :default
          (mm/mul m a))))

(extend-protocol mp/PVectorTransform
  clojure.lang.PersistentVector
    (vector-transform [m a]
      (mp/matrix-multiply m a))
    (vector-transform! [m a]
      (mp/assign! a (mp/matrix-multiply m a))))

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
      (vector-dimensionality m))
    (is-vector? [m]
      (== 1 (mp/dimensionality m)))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (let [c (long (count m))]
        (cons c (if (> c 0) 
                  (mp/get-shape (m 0))
                  nil)))) 
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
