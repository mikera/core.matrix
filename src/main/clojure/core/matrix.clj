(ns core.matrix
  (:use core.matrix.utils)
  (:use core.matrix.protocols)
  (:require [core.matrix.multimethods :as mm])
  (:require [core.matrix.impl.mathsops :as mops]))


;; =============================================================
;; API protocols
;;
;; Matrix implementations should extend all of these for full API support

;; protocol arity overloads behave oddly, so different names used for simplicity
;; we provide fast paths for 1D and 2D access (common case)
(defprotocol PIndexedAccess
  "Protocol for indexed read access to matrices and vectors."
  (get-1d [m x])
  (get-2d [m x y])
  (get-nd [m indexes]))

(defprotocol PCoercion
  "Protocol to coerce a parameter to a format usable by a specific implementation. It is 
   up to the implementation to determine what parameter types they support" 
  (coerce-param [m param]))

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply [m a])
  (scale [m a]))

(defprotocol PMatrixAdd
  "Protocol to support matrix addition on an arbitrary matrices of same size"
  (matrix-add [m a])
  (matrix-sub [m a]))

(defprotocol PVectorOps
  "Protocol to support common vector operations"
  (vector-dot [a b])
  (length-squared [a])
  (normalise [a]))

(defprotocol PMatrixOps
  "Protocol to support common matrix operations"
  (trace [m])
  (determinant [m])
  (inverse [m])
  (negate [m])
  (transpose [m]))

;; code generation for protocol with unary mathematics operations defined in c.m.i.mathsops namespace
(eval
  `(defprotocol PMathsFunctions
  "Protocol to support mathematic functions applied element-wise to a matrix"
  ~@(map (fn [[name func]] `(~name [~'m])) mops/maths-ops)))

(defprotocol PMatrixSlices
  "Protocol to support getting slices of a matrix"
  (get-row [m i])
  (get-column [m i]))

(defprotocol PDimensionInfo
  "Protocol to return standard dimension information about a matrix"
  (dimensionality [m])
  (is-vector? [m])
  (row-count [m])
  (column-count [m])
  (dimension-count [m x]))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers."
  (convert-to-nested-vectors [m]))

;; =============================================================
;; Functions operating on standard protocols
;;
;; API users should probably prefer these functions to using the protocols directly?

(defn matrix? 
  "Returns true if parameter is a valid matrix (any dimensionality)"
  ([m]
    (satisfies? PDimensionInfo m)))

(defn matrix-2d? 
  "Returns true if parameter is a regular matrix (2 dimensional matrix)"
  ([m]
    (and (matrix? m) (== 2 (dimensionality m)))))

(defn matrix-1d? 
  "Returns true if parameter is a 1 dimensional matrix"
  ([m]
    (and (matrix? m) (== 1 (dimensionality m)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix"
  ([m]
    (== 1 (row-count m))))

(defn square?
  "Returns true if matrix is square (same number of rows and columns)"
  ([m]
    (== (row-count m) (column-count m))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix"
  ([m]
    (== 1 (column-count m))))

(defn all-dimensions
  "Returns a sequence of the dimension counts for a matrix"
  ([m]
    (for [i (range (dimensionality m))] (dimension-count m i))))

(defn mget 
  "Gets a value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m x]
    (get-1d m x))
  ([m x y]
    (get-2d m x y))
  ([m x y & more]
    (get-nd m (cons x (cons y more)))))

(defn coerce
  "Coerces a parameter to a format usable by a specific matrix implementation"
  ([m a]
    (or 
      (coerce-param m a)
      (coerce-param m (convert-to-nested-vectors a)))))

(defn mul
  "Performs matrix multiplication with matrices, vectors or scalars"
  ([a] a)
  ([a b]
    (cond 
      (number? b) (if (number? a) (* a b) (scale a b))
      (number? a) (scale b a)
      :else (matrix-multiply a b)))
  ([a b & more]
    (reduce mul (mul a b) more)))

(defn add
  "Performs matrix addition on two matrices of same size"
  ([a] a)
  ([a b] 
    (matrix-add a b))
  ([a b & more]
    (reduce matrix-add (matrix-add a b) more)))

(defn sub
  "Performs matrix subtraction on two matrices of same size"
  ([a] a)
  ([a b] 
    (matrix-sub a b))
  ([a b & more]
    (reduce matrix-sub (matrix-sub a b) more)))

(defn dot
  "Computes the dot product of two vectors"
  ([a b]
    (vector-dot a b)))

(defn det
  "Calculates the determinant of a matrix"
  ([a]
    (determinant a)))


;; ============================================================
;; Fallback implementations 
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise often we can implement in terms of simpler operations.

;; default implementation for matrix ops
(extend-protocol PMatrixOps
  java.lang.Object
    (trace [m]
      (if-not (square? m) (error "Can't compute trace of non-square matrix"))
	    (let [dims (long (row-count m))]
	      (loop [i 0 res 0.0]
	        (if (>= i dims)
	          res
	          (recur (inc i) (+ res (double (get-2d m i i))))))))
    (negate [m]
      (scale m -1.0)))

;; support indexed gets on any kind of java.util.List
(extend-protocol PIndexedAccess
  java.lang.Number
    (get-1d [m x]
      (error "Scalar has zero dimensionality!"))
    (get-2d [m x y]
      (error "Scalar has zero dimensionality!"))
    (get-nd [m indexes]
      (if (seq indexes)
        (error "Scalar has zero dimensionality!")
        m))
  java.util.List
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (get-1d (.get m (int x)) y))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.get m (int (first indexes)))]
          (get-nd m next-indexes))
        (.get m (int (first indexes))))))

;; matrix multiply for scalars
(extend-protocol PMatrixMultiply
  java.lang.Number
    (matrix-multiply [m a]
      (if (number? a) 
        (* m a)
        (scale a m)))
    (scale [m a]
      (if (number? a) 
        (* m a)
        (scale a m))))

;; matrix add for scalars
(extend-protocol PMatrixAdd
  java.lang.Number
    (matrix-add [m a]
      (if (number? a) (+ m a) (error "Can't add scalar number to a matrix")))
    (matrix-sub [m a]
      (if (number? a) (- m a) (error "Can't a matrix from a scalar number"))))

;; attempt conversion to nested vectors
(extend-protocol PConversion
  java.lang.Object
    (convert-to-nested-vectors [m]
      (if (vector? m)
        (mapv #(mget m %) (range (row-count m))))
        (mapv #(convert-to-nested-vectors (get-row m %)) (range (column-count m)))))

;; define standard Java maths functions for numbers
(eval
  `(extend-protocol PMathsFunctions
     java.lang.Number
       ~@(map (fn [[name func]]
                `(~name [~'m] (double (~func (double ~'m)))))
              mops/maths-ops)))

;; =======================================================
;; default multimethod implementations


(defmethod mm/mul :default [x y]
  (error "Don't know how to multiply " (class x) " with " (class y)))

;; =========================================================
;; Final setup


