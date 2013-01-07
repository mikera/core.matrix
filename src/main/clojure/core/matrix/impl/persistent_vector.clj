(ns core.matrix.impl.persistent-vector
  (:use core.matrix)
  (:refer-clojure :exclude [vector?]))

;; =======================================================================
;; utility functions for manipulating persistent vector matrices

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

(extend-protocol PIndexedAccess
  clojure.lang.IPersistentVector
    (get-1d [m x]
      (.nth m (int x)))
    (get-2d [m x y]
      (let [row (.nth m (int x))]
        (get-1d row y)))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.nth m (int (first indexes)))]
          (get-nd m next-indexes))
        (.nth m (int (first indexes))))))

(extend-protocol PMatrixSlices
  clojure.lang.IPersistentVector
    (get-row [m i]
      (.nth m (long i)))
    (get-column [m i]
      (let [i (long i)]
        (mapv #(nth % i) m))))

(extend-protocol PMatrixAdd
  clojure.lang.IPersistentVector
    (matrix-add [m a]
      (mapmatrix + m (coerce m a)))
    (matrix-sub [m a]
      (mapmatrix - m (coerce m a))))

(extend-protocol PVectorOps
  clojure.lang.IPersistentVector
    (vector-dot [a b]
      (reduce + 0 (map (fn [x y] (* x y)) a (coerce a b))))
    (length-squared [a]
      (reduce + (map #(* % %) a)))
    (normalise [a]
      (scale a (/ 1.0 (Math/sqrt (length-squared a))))))

(extend-protocol PCoercion
  clojure.lang.IPersistentVector
    (coerce-param [m param]
      (cond
        (clojure.core/vector? param) param
        (sequential? param) (coerce-nested param)
        (instance? java.util.List param) (coerce-nested param)
        (instance? java.lang.Iterable param) (coerce-nested param)
        :default (error "Can't coerce to vector: " (class param)))))

(extend-protocol PMatrixMultiply
  clojure.lang.IPersistentVector
    (matrix-multiply [m a]
      (let [a (coerce-param m a)]
        (error "Not yet implemented")))
    (scale [m a]
      (let [a (double a)]
        (mapmatrix (partial * a) m))))

;; helper functin to build generic maths operations
(defn build-maths-function 
  ([[name func]]
    `(~name [~'m]
            (mapmatrix (fn [x#] (double (~func (double x#)))) ~'m))))

(def maths-ops
  '[(exp Math/exp)
	  (abs Math/abs)
	  (acos Math/acos)
	  (asin Math/asin)
	  (atan Math/atan)
	  (cbrt Math/cbrt)
	  (ceil Math/ceil)
	  (cos Math/cos)
	  (cosh Math/cosh)
	  (exp Math/exp)
	  (floor Math/floor)
	  (log Math/log)
	  (log10 Math/log)
	  (signum Math/signum)
	  (sin Math/sin)
	  (sinh Math/sinh)
	  (sqrt Math/sqrt)
	  (tan Math/tan)
	  (tanh Math/tanh)])

(eval
  `(extend-protocol PMathsFunctions
     clojure.lang.IPersistentVector
       ~@(map build-maths-function maths-ops)
     java.lang.Number
       ~@(map (fn [[name func]]
                `(~name [~'m] (double (~func (double ~'m)))))
              maths-ops)))

(extend-protocol PMatrixDimensionInfo
  clojure.lang.IPersistentVector
    (dimensionality [m]
      (let [fst (.get m 0)]
        (if (number? fst) 
          1
          (inc (dimensionality fst)))))
    (row-count [m]
      (count m))
    (is-vector? [m]
      (number? (m 0)))
    (column-count [m]
      (let [x (m 0)] 
        (if (number? x) 
          1 
          (count (m 0)))))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (dimension-count (m 0) (dec x)))))

