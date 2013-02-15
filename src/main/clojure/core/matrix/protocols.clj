(ns clojure.core.matrix.protocols
  (:require [clojure.core.matrix.impl.mathsops :as mops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ================================================================
;; clojure.core.matrix API protocols
;;
;; Matrix implementations should extend these for full API support
;;
;; This namespace is intended for use by API implementers only
;; clojure.core.matrix users should not access these protocols directly
;;
;; ================================================================

;; ===================================================================================
;; MANDATORY PROTOCOLS FOR ALL IMPLEMENTATIONS
;;
;; A compliant clojure.core.matrix implementation must implement these.
;; Otherwise things will fail.

(defprotocol PImplementation
  "Protocol for general implementation functionality. Required to support implementation metadata and
   matrix construction."
  (implementation-key [m]
    "Returns a keyword representing this implementation.
     Each implementation should have one unique key.")
  (construct-matrix [m data]
    "Returns a new matrix containing the given data. data should be in the form of either
     nested sequences or a valid existing matrix")
  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length.")
  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and columns.")
  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Shape must be a sequence of dimension sizes.")
  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number of dimensions."))

(defprotocol PDimensionInfo
  "Protocol to return standard dimension information about a matrix.
   dimensionality and dimension-count are mandatory for implementations"
  (dimensionality [m]
    "Returns the number of dimensions of a matrix")
  (get-shape [m]
    "Returns the shape of the matrix, as an array or sequence of dimension sizes")
  (is-scalar? [m]
    "Tests whether an object is a scalar value")
  (is-vector? [m]
    "Tests whether an object is a vector (1D matrix)")
  (dimension-count [m dimension-number]
    "Returns the size of a specific dimension "))

;; protocol arity overloads behave oddly, so different names used for simplicity
;; we provide fast paths for 1D and 2D access (common case)
(defprotocol PIndexedAccess
  "Protocol for indexed read access to matrices and vectors."
  (get-1d [m row])
  (get-2d [m row column])
  (get-nd [m indexes]))

;; ===================================================================================
;; MANDATORY PROTOCOLS FOR MUTABLE MATRICES
;;
;; A compliant clojure.core.matrix mutable implementation must implement these.
;; Otherwise things will fail.

(defprotocol PIndexedSetting
  "Protocol for indexed setter access to matrices and vectors.
   Must be supported for any mutable matrix type."
  (set-1d [m row v])
  (set-2d [m row column v])
  (set-nd [m indexes v])
  (is-mutable? [m]))

(defprotocol PMatrixCloning
  "Protocol for cloning a matrix value."
  (clone [m] "Returns a clone of an array. Must be a new independent (non-view)
              instance if the array is mutable."))

;; ===================================================================================
;; OPTTIONAL PROTOCOLS
;;
;; implementations don't need to provide these since fallback default implementations
;; are provided. However, they should consider doing so for performance reasons


(defprotocol PTypeInfo
  "Protocol for querying the type of matrix elements. If not provided, the default implementation will
   examine the first element in the matrix to determone the type."
  (element-type [m]))

(defprotocol PSpecialisedConstructors
  "Protocol for construction of special matrices."
  (identity-matrix [m dims] "Create a 2D identity matrix with the given number of dimensions")
  (diagonal-matrix [m diagonal-values] "Create a diagonal matrix with the specified leading diagonal values"))

(defprotocol PCoercion
  "Protocol to coerce a parameter to a format usable by a specific implementation. It is
   up to the implementation to determine what parameter types they support. If the
   implementation is unable to perform coercion, it may return nil.
   Implementations should also be able to coerce scalar values."
  (coerce-param [m param]
    "Attempts to coerce param into a matrix format supported by the implementation of matrix m.
     May return nil if unable to do so, in which case a default implementation can be used."))

(defprotocol PBroadcast
  "Protocol to support broadcasting over one or more dimensions."
  (broadcast [m target-shape]
    "Broadcasts an array over a desired target shape, which should be larger than the current matrix.
     Dimensions should be matched up according to the last dimension.
     In order to broadcast sucessfully, the current dimension of the array must be either:
      - of size 1
      - equal to the size of the dimension in the target shape
      - not included in the array (i.e. the target shape has more leading dimensions)"))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers."
  (convert-to-nested-vectors [m]))

(defprotocol PReshaping
  "Protocol to reshape matrices. Must support any new shape allowed by the implementation.
   Must preserve row-major ordering of matrix elements.
   If the new shape has less elements than the original shape, it is OK to truncate the remaining elements.
   If the new shape requires more elements than the original shape, should throw an exception."
  (reshape [m shape]))


(defprotocol PMatrixSlices
  "Protocol to support getting slices of an array.
   Functions should return either the actual components or a mutable view if possible,
   i.e. making a full copy should be avoided."
  (get-row [m i])
  (get-column [m i])
  (get-major-slice [m i])
  (get-slice [m dimension i]))

(defprotocol PSliceView
  "Protocol for quick view access into a row-major slices of an array. If implemented, must return either a view
   or an immutable sub-matrix: it must *not* return copied data. 
   The default implementation creates a wrapper view."
  (get-major-slice-view [m i] "Gets a view of a major array slice"))

(defprotocol PSliceSeq
  "Returns the row-major slices of the matrix as a sequence. Ideally these should be views or immutable sub-arrays.
   The default implementation uses get-major-slice-view to obtain the slices."
  (get-major-slice-seq [m] "Gets a sequence of all major array slices"))

(defprotocol PMatrixSubComponents
  "Protocol for picking out subsections of a matrix. Should return a mutable view if possible.
   The default implementation creates a new vector containing the diagonal values." 
  (main-diagonal [m]))


(defprotocol PAssignment
  "Protocol for assigning values to mutable matrices."
  (assign! 
    [m source] 
    "Sets all the values in a matrix from a matrix source")
  (assign-array!
    [m arr]
    [m arr start length]
    "Sets all the values in a matrix for an array source."))

(defprotocol PDoubleArrayOutput
  "Protocol for getting data as a double array"
  (to-double-array [m]
    "Returns a double array containing the values of m in row-major order. May or may not be
     the internal double array used by m, depending on the implementation.")
  (as-double-array [m]
    "Returns the internal double array used by m. If no such array is used, returns nil.
     Provides an opportunity to avoid copying the internal array.")) 

(defprotocol PMatrixEquality
  "Protocol for matrix equality operations"
  (matrix-equals [a b]
     "Return true if a equals b, i.e. if all elements are equal.
      Must use numerical value comparison on numbers (==) to account for matrices that may hold a mix of
      numercial types (e.g. java.lang.Long and java.lang.Double)"))

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply [m a])
  (element-multiply [m a]))

(defprotocol PVectorTransform
  "Protocol to support transformation of a vector to another vector.
   Is equivalent to matrix multiplication when 2D matrices are used as transformations.
   But other transformations are possible, e.g. affine transformations."
  (vector-transform [m v] "Transforms a vector")
  (vector-transform! [m v] "Transforms a vector in place - mutates the vector argument"))

(defprotocol PMatrixScaling
  "Protocol to support matrix scaling by scalar values"
  (scale [m a]
    "Multiplies a matrix by the scalar value a")
  (pre-scale [m a]
    "Pre-multiplies the matrix with the scalar. This is the same as scale for matrices 
     where multiplication is commutative, but may be different for special kinds of scalars."))

(defprotocol PMatrixMutableScaling
  "Protocol to support matrix scaling by scalar values"
  (scale! [m a])
  (pre-scale! [m a]))

(defprotocol PMatrixAdd
  "Protocol to support matrix addition on an arbitrary matrices of same size"
  (matrix-add [m a])
  (matrix-sub [m a]))

(defprotocol PVectorOps
  "Protocol to support common vector operations."
  (vector-dot [a b]
     "Dot product of two vectors. Should return a scalar value.")
  (length [a]
     "Euclidian length of a vector.")
  (length-squared [a]
     "Squared Euclidean length of a vector.")
  (normalise [a]
     "Returns a new vector, normalised to length 1.0"))

(defprotocol PMutableVectorOps
  "Protocol for mutable versions of commn vector operations" 
  (normalise! [a]))

(defprotocol PMatrixOps
  "Protocol to support common matrix operations"
  (trace [m]
    "Returns the trace of a matrix (sum of elements on main diagonal.
     Must throw an error if the matrix is not square (i.e. all dimensions sizes are equal)")
  (determinant [m]
    "Returns the determinant of a matrix.")
  (inverse [m]
    "Returns the invese of a matrix. Should throw an exception if m is not invertible.")
  (negate [m]
    "Returns a new matrix with all elements negated.")
  (transpose [m]
    "Returns the transpose of a matrix. Equivalent to reversing the \"shape\".
     Note that:
     - The transpose of a scalar is the same scalar
     - The transpose of a 1D vector is the same 1D vector
     - The transpose of a 2D matrix swaps rows and columns"))

(defprotocol PSummable
  "Protocol to support the summing of all elements in an array. 
   The array must hold numeric values only, or an exception will be thrown."
  (sum [m]))

;; code generation for protocol with unary mathematics operations defined in c.m.i.mathsops namespace
;; also generate in-place versions e.g. signum!
(eval
  `(defprotocol PMathsFunctions
  "Protocol to support mathematic functions applied element-wise to a matrix"
  ~@(map (fn [[name func]] `(~name [~'m])) mops/maths-ops)
  ~@(map (fn [[name func]] `(~(symbol (str name "!")) [~'m])) mops/maths-ops)))


(defprotocol PFunctionalOperations
  "Protocol to allow functional-style operations on matrix elements."
  ;; note that protocols don't like variadic args, so we convert to regular args
  ;; also the matrix type must be first for protocol dispatch, so we move it before f
  (element-seq 
    [m]
    "Must return a sequence containing all elements of the matrix, in row-major order.")
  (element-map 
    [m f]        
    [m f a]              
    [m f a more]
    "Maps f over all elements of m (and optionally other matrices), returning a new matrix")
  (element-map! 
    [m f]           
    [m f a]
    [m f a more]
    "Maps f over all elements of m (and optionally other matrices), mutating the elements of m in place.
     Must throw an exception if m is not mutable.")
  (element-reduce 
    [m f] 
    [m f init]
    "Reduces over all elements of m."))
