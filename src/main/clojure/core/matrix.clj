(ns core.matrix
  (:use core.matrix.utils)
  (:require [core.matrix.protocols :as mp])
  (:require [core.matrix.multimethods :as mm])
  (:require [core.matrix.impl.mathsops :as mops]))

;; ==================================================================================
;; core.matrix API namespace
;;
;; This is the public API for core.matrix
;;
;; General handling of operations is as follows
;; 1. user calls public AI function defined in core.matrix
;; 2. core.matrix function delegates to a protocol for the appropriate function
;;    with protocols as defined in the core.matrix.protocols namespace
;; 3. The underlying matrix implementation implements the protocol to handle the API
;;    function call
;; 4. It's up to the implementation to decide what to do then
;; ==================================================================================

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =============================================================
;; matrix construction functions

(def ^:dynamic *matrix-implementation* nil)

(defn matrix 
  "Constructs a matrix from the given data. The data should be in the form of nested sequences.

   Uses the current matrix library as specified in *matrix-implementation*"
  ([data]
    (error "not yet implemented")))

(defn clone
  "Constructs a clone of the matrix, using the same implementation. This function is intended to
   allow safe defensive copying of matrices / vectors.

   Guarantees that:
   1. Mutating the returned matrix will not modify any other matrix (defensive copy)
   2. The return matrix will be mutable, if the implementation supports mutable matrices.

   A matrix implementation which only provides immutable matrices may safely return the same matrix."
  ([m]
    (error "not yet implemented")))

;; =============================================================
;; Functions operating on standard protocols
;;
;; API users should probably prefer these functions to using the protocols directly?

(defn matrix? 
  "Returns true if parameter is a valid matrix (any dimensionality)"
  ([m]
    (> (mp/dimensionality m) 0)))

(defn vec?
  "Returns true if the parameter is a vector"
  ([m]
    (mp/is-vector? m)))

(defn dimensionality
  "Returns the dimensionality (number of array dimensions) of a matrix"
  ([m]
    (mp/dimensionality m)))

(defn row-count
  "Returns the number of rows in a matrix"
  ([m]
    (mp/dimension-count m 0)))

(defn column-count
  "Returns the number of columns in a matrix"
  ([m]
    (mp/dimension-count m 1)))

(defn matrix-2d? 
  "Returns true if parameter is a regular matrix (2 dimensional matrix)"
  ([m]
    (and (matrix? m) (== 2 (mp/dimensionality m)))))

(defn matrix-1d? 
  "Returns true if parameter is a 1 dimensional matrix"
  ([m]
    (and (matrix? m) (== 1 (mp/dimensionality m)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix"
  ([m]
    (== 1 (mp/dimension-count m 0))))

(defn square?
  "Returns true if matrix is square (same number of rows and columns)"
  ([m]
    (== (mp/dimension-count m 0) (mp/dimension-count m 1))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix"
  ([m]
    (== 1 (mp/dimension-count m 1))))

(defn all-dimensions
  "Returns a sequence of the dimension counts for a matrix"
  ([m]
    (for [i (range (mp/dimensionality m))] (mp/dimension-count m i))))

(defn mget 
  "Gets a value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m]
    m)
  ([m x]
    (mp/get-1d m x))
  ([m x y]
    (mp/get-2d m x y))
  ([m x y & more]
    (mp/get-nd m (cons x (cons y more)))))

(defn get-row
  "Gets a row of a 2D matrix"
  ([m x]
    (mp/get-row m x)))

(defn get-column
  "Gets a column of a 2D matrix"
  ([m y]
    (mp/get-column m y)))

(defn coerce
  "Coerces a parameter to a format usable by a specific matrix implementation"
  ([m a]
    (or 
      (mp/coerce-param m a)
      (mp/coerce-param m (mp/convert-to-nested-vectors a)))))

(defn mul
  "Performs matrix multiplication with matrices, vectors or scalars"
  ([a] a)
  ([a b]
    (cond 
      (number? b) (if (number? a) (* a b) (mp/scale a b))
      (number? a) (mp/scale b a)
      :else (mp/matrix-multiply a b)))
  ([a b & more]
    (reduce mul (mul a b) more)))

(defn add
  "Performs matrix addition on two matrices of same size"
  ([a] a)
  ([a b] 
    (mp/matrix-add a b))
  ([a b & more]
    (reduce mp/matrix-add (mp/matrix-add a b) more)))

(defn sub
  "Performs matrix subtraction on two matrices of same size"
  ([a] a)
  ([a b] 
    (mp/matrix-sub a b))
  ([a b & more]
    (reduce mp/matrix-sub (mp/matrix-sub a b) more)))

(defn scale
  "Scales a matrix by a scalar factor"
  ([m factor]
    (mp/scale m factor)))

(defn normalise
  "Normalises a matrix (scales to unit length)"
  ([m]
    (mp/normalise m)))

(defn dot
  "Computes the dot product of two vectors"
  ([a b]
    (mp/vector-dot a b)))

(defn det
  "Calculates the determinant of a matrix"
  ([a]
    (mp/determinant a)))


(defn trace
  "Calculates the trace of a matrix (sum of elements on main diagonal)"
  ([a]
    (mp/trace a)))


(defn length
  "Calculates the length (magnitude) of a vector"
  ([m]
    (mp/length m)))

(defn length-squared
  "Calculates the squared length (squared magnitude) of a vector"
  ([m]
    (mp/length-squared m)))

;; create all unary maths operators
(eval
  `(do ~@(map (fn [[name func]] 
           `(defn ~name 
              ([~'m]
                (~(symbol "core.matrix.protocols" (str name)) ~'m)))) mops/maths-ops)))

;; ============================================================
;; Fallback implementations 
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise often we can implement in terms of simpler operations.

;; default implementation for matrix ops

(extend-protocol mp/PIndexedAccess
  java.util.List
    (get-1d [m x]
      (.get m (int x)))
    (get-2d [m x y]
      (mp/get-1d (.get m (int x)) y))
    (get-nd [m indexes]
      (if-let [s (seq indexes)] 
        (mp/get-nd (.get m (int (first s))) (next s))
        m))
  java.lang.Object
    (get-1d [m x] (error "Can't get-1d from an object that has no dimensions"))
    (get-2d [m x y] (error "Can't get-2d from an object that has no dimensions"))
    (get-nd [m indexes] 
      (if (seq indexes)
        (error "Can't get from an object that has no dimensions")
        m)))

;; support indexed gets on any kind of java.util.List
(extend-protocol mp/PDimensionInfo
  java.lang.Object
    (dimensionality [m] 0)
    (dimension-count [m i] 1))


(extend-protocol mp/PMatrixOps
  java.lang.Object
    (trace [m]
      (if-not (square? m) (error "Can't compute trace of non-square matrix"))
	    (let [dims (long (row-count m))]
	      (loop [i 0 res 0.0]
	        (if (>= i dims)
	          res
	          (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (negate [m]
      (mp/scale m -1.0))
    (length-squared [m]
      (reduce #(+ %1 (mget m %2)) 0 (range (row-count m))))
    (length [m]
      (Math/sqrt (mp/length-squared m))))

(extend-protocol mp/PMatrixOps
  java.lang.Object
    (trace [m]
      (if-not (square? m) (error "Can't compute trace of non-square matrix"))
	    (let [dims (long (row-count m))]
	      (loop [i 0 res 0.0]
	        (if (>= i dims)
	          res
	          (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (negate [m]
      (mp/scale m -1.0))
    (length-squared [m]
      (reduce #(+ %1 (mget m %2)) 0 (range (row-count m))))
    (length [m]
      (Math/sqrt (mp/length-squared m))))

;; matrix multiply for scalars
(extend-protocol mp/PMatrixMultiply
  java.lang.Number
    (matrix-multiply [m a]
      (if (number? a) 
        (* m a)
        (mp/scale a m)))
    (scale [m a]
      (if (number? a) 
        (* m a)
        (mp/scale a m))))

;; matrix add for scalars
(extend-protocol mp/PMatrixAdd
  java.lang.Number
    (matrix-add [m a]
      (if (number? a) (+ m a) (error "Can't add scalar number to a matrix")))
    (matrix-sub [m a]
      (if (number? a) (- m a) (error "Can't a matrix from a scalar number"))))

;; attempt conversion to nested vectors
(extend-protocol mp/PConversion
  java.lang.Object
    (convert-to-nested-vectors [m]
      (if (vector? m)
        (mapv #(mget m %) (range (row-count m))))
        (mapv #(mp/convert-to-nested-vectors (mp/get-row m %)) (range (column-count m)))))

;; define standard Java maths functions for numbers
(eval
  `(extend-protocol mp/PMathsFunctions
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


