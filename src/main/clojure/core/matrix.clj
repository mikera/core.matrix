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
;; 5. If the implementation does not understand one or more parameters, then it is
;;    expected to call the multimethod version in core.matrix.multimethods as this
;;    will allow an alternative implementation to be found via multiple dispatch  
;; 
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

;; ==============================
;; Matrix predicates and querying

(defn matrix? 
  "Returns true if parameter is a valid matrix (any dimensionality)"
  ([m]
    (> (mp/dimensionality m) 0)))

(defn vec?
  "Returns true if the parameter is a vector"
  ([m]
    (mp/is-vector? m)))

(defn scalar? 
  "Returns true if the parameter is a scalar (has zero dimensionality)."
  ([m]
    (mp/is-scalar? m)))

(defn dimensionality
  "Returns the dimensionality (number of array dimensions) of a matrix / array"
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
    (== 2 (mp/dimensionality m))))

(defn matrix-1d? 
  "Returns true if parameter is a 1 dimensional matrix"
  ([m]
    (== 1 (mp/dimensionality m))))

(defn square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. has exactly one row)"
  ([m]
    (and (>= (mp/dimensionality m) 1) 
         (== 1 (mp/dimension-count m 0)))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. has exactly one column)"
  ([m]
    (or (== (mp/dimensionality m) 1)
        (== 1 (mp/dimension-count m 1)))))

(defn dimensions
  "Returns all the dimension sizes for a matrix, as a sequable result.
   Result may be a sequence or Java array."
  ([m]
    (for [i (range (mp/dimensionality m))] (mp/dimension-count m i))))

;; =======================================
;; matrix access

(defn mget 
  "Gets a scalar value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m]
    (if (mp/is-scalar? m) 
      m 
      (error "Can't mget from a non-scalar value without indexes")))
  ([m x]
    (mp/get-1d m x))
  ([m x y]
    (mp/get-2d m x y))
  ([m x y & more]
    (mp/get-nd m (cons x (cons y more)))))

(defn mset! 
  "Sets a scalar value in a matrix at a specified position. Supports any number of matrix dimensions.
   Will throw an error if the matrix is not mutable."
  ([m v]
    (if (mp/is-scalar? m) 
      (error "Can't set a scalar value!") 
      (error "Can't mset a non-scalar value without indexes")))
  ([m x v]
    (mp/set-1d m x v))
  ([m x y v]
    (mp/set-2d m x y v))
  ([m x y z & more]
    (mp/set-nd m (cons x (cons y (cons z (butlast more)))) (last more))))

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

;; =====================================
;; matrix slicing and views

(defn sub-matrix 
  "Gets a view of a submatrix, for a set of index-ranges.
   Index ranges should be [start, length] pairs.
   Index ranges can be nil (gets the whole range) "
  ([m index-ranges]
    (TODO)))

(defn sub-vector
  "Gets a view of part of a vector. The view maintains a reference to the original,
   so can be used to modify the original vector if it is mutable."
  ([m start length]
    (TODO)))

(defn slice 
  "Gets a view of a slice of a matrix along a specific dimension.
   The returned matrix will have one less dimension. 
   Slicing a 1D vector will return a scalar.

   Slicing on the first dimension (dimension 0) is likely to perform better
   for many matrix implementations."
  ([m dimension index]
    (TODO)))

;; ======================================
;; matrix maths / operations

(defn mul
  "Performs matrix multiplication with matrices, vectors or scalars"
  ([a] a)
  ([a b]
    (cond 
      (scalar? b) (if (scalar? a) (* a b) (mp/scale a b))
      (scalar? a) (mp/pre-scale b a)
      :else (mp/matrix-multiply a b)))
  ([a b & more]
    (reduce mul (mul a b) more)))

(defn emul
  "Performs element-wise matrix multiplication. Matrices must be same size."
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce mp/element-multiply (mp/element-multiply a b) more)))

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

;; ====================================
;; functional operations

(defn eseq
  "Returns all elements of a matrix as a sequence in row-major order"
  ([m]
    (mp/element-seq m)))

(defn ereduce 
  "Element-wise reduce on all elements of a matrix."
  ([f m]
    (mp/element-reduce m f))
  ([f init m]
    (mp/element-reduce m f init)))
    
(defn emap 
  "Element-wise map over all elements of one or more matrices. 
   Returns a new matrix of the same type."
  ([f m]
    (mp/element-map m f))
  ([f m a]
    (mp/element-map m f a))
  ([f m a & more]
    (mp/element-map m f a more)))

(defn emap! 
  "Element-wise map over all elements of one or more matrices. 
   Performs in-place modification of the first matrix argument."
  ([f m]
    (mp/element-map! m f))
  ([f m a]
    (mp/element-map! m f a))
  ([f m a & more]
    (mp/element-map! m f a more)))

;; ============================================================
;; Fallback implementations 
;; - default behaviour for java.lang.Number scalars
;; - for stuff we don't recognise (java.lang.Object) we should try to 
;;   implement in terms of simpler operations, on assumption that
;;   we have fallen through to the default implementation

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
    (get-1d [m x] (mp/get-nd m [x]))
    (get-2d [m x y] (mp/get-nd m [x y]))
    (get-nd [m indexes] 
      (if (seq indexes)
        (error "Can't determine dimensionality of:" (class m))
        (if (scalar? m) m
          (error "Not a scalar, cannot do zero dimensional get")))))

;; support indexed gets on any kind of java.util.List
(extend-protocol mp/PDimensionInfo
  java.lang.Number
    (dimensionality [m] 0)
    (is-scalar? [m] true)
    (is-vector? [m] false) 
    (dimension-count [m i] (error "java.lang.Number has zero dimensionality, cannot get dimension count"))
  java.lang.Object
    (dimensionality [m] (error "Can't determine dimensionality: " (class m)))
    (is-vector? [m] (== 1 (mp/dimensionality m))) 
    (is-scalar? [m] (== 0 (mp/dimensionality m))) 
    (dimension-count [m i] (error "Can't determine count of dimension " i " on object " (class m))))

;; generic versions of matrix ops
(extend-protocol mp/PMatrixOps
  java.lang.Object
    (trace [m]
      (when-not (square? m) (error "Can't compute trace of non-square matrix"))
	    (let [dims (long (row-count m))]
	      (loop [i 0 res 0.0]
	        (if (>= i dims)
	          res
	          (recur (inc i) (+ res (double (mp/get-2d m i i))))))))
    (negate [m]
      (mp/scale m -1.0))
    (length-squared [m]
      (ereduce #(+ %1 (* %2 *2)) 0.0 m))
    (length [m]
      (Math/sqrt (mp/length-squared m))))

;; matrix multiply
(extend-protocol mp/PMatrixMultiply
  java.lang.Number
    (element-multiply [m a]
      (clojure.core/* m a))
    (matrix-multiply [m a]
      (if (number? a) 
        (* m a)
        (mp/pre-scale a m)))
  java.lang.Object
    (element-multiply [m a]
      (emap clojure.core/* m a)))

;; matrix scaling
(extend-protocol mp/PMatrixScaling
  java.lang.Number
    (scale [m a]
      (if (number? a) 
        (* m a)
        (mp/pre-scale a m)))
    (pre-scale [m a]
      (if (number? a) 
        (* a m)
        (mp/scale a m)))
  java.lang.Object
    (scale [m a]
      (emap #(* % a) m))
    (pre-scale [m a]
      (emap (partial * a) m)))

(extend-protocol mp/PMatrixAdd
  ;; matrix add for scalars
  java.lang.Number
    (matrix-add [m a]
      (if (number? a) (+ m a) (error "Can't add scalar number to a matrix")))
    (matrix-sub [m a]
      (if (number? a) (- m a) (error "Can't a matrix from a scalar number"))))

;; functional operations
(extend-protocol mp/PFunctionalOperations
  java.lang.Number
    (element-seq [m]
      (list m))
    (element-map 
      ([m f]
        (list (f m)))
      ([m f a]
        (list (f m a)))
      ([m f a more]
        (list (apply f m a more))))
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
        (f init m))))

;; attempt conversion to nested vectors
(extend-protocol mp/PConversion
  java.lang.Number
    (convert-to-nested-vectors [m]
      ;; we accept a scalar as a "nested vector" for these purposes?
      m) 
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
              mops/maths-ops)
     java.lang.Object
       ~@(map (fn [[name func]]
                `(~name [~'m] (emap #(double (~func (double %))) ~'m)))
              mops/maths-ops)
       ~@(map (fn [[name func]]
                `(~(symbol (str name "!")) [~'m] (emap! #(double (~func (double %))) ~'m)))
              mops/maths-ops)))

;; =======================================================
;; default multimethod implementations


(defmethod mm/mul :default [x y]
  (error "Don't know how to multiply " (class x) " with " (class y)))

;; =========================================================
;; Final setup


