(ns core.matrix.protocols
  (:require [core.matrix.impl.mathsops :as mops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ================================================================
;; core.matrix API protocols
;;
;; Matrix implementations should extend these for full API support
;;
;; This namespace is intended for use by API implementers only
;; core.matrix users should not access these protocols directly
;;
;; ================================================================

;; ===================================================================================
;; MANDATORY PROTOCOLS FOR ALL IMPLEMENTATIONS
;;
;; A compliant core.matrix implementation must implement these.
;; Otherwise things will fail.

(defprotocol PImplementation
  "Protocol for general implementation functionality"
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
;; A compliant core.matrix mutable implementation must implement these.
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

(defprotocol PAssignment
  "Protocol for assigning values to mutable matrices."
  (assign! [m source] "Sets all the values in a matrix from a matrix source")
  (assign-array!
    [m arr]
    [m arr start length]))

(defprotocol PMatrixEquality
  "Protocol for matrix equality operations"
  (matrix-equals [a b]))

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
  (scale [m a])
  (pre-scale [m a]))

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
  (vector-dot [a b])
  (length [a])
  (length-squared [a])
  (normalise [a]))

(defprotocol PMutableVectorOps
  "Protocol for mutable versions of commn vector operations" 
  (normalise! [a]))

(defprotocol PMatrixOps
  "Protocol to support common matrix operations"
  (trace [m])
  (determinant [m])
  (inverse [m])
  (negate [m])
  (transpose [m]))

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

(defprotocol PMatrixSlices
  "Protocol to support getting slices of an array.
   Functions should return either the actual components or a mutable view if possible,
   i.e. making a full copy should be avoided."
  (get-row [m i])
  (get-column [m i])
  (get-major-slice [m i])
  (get-slice [m dimension i]))

(defprotocol PMatrixSubComponents
  "Protocol for picking out subsections of a matrix" 
  (main-diagonal [m]))

(defprotocol PFunctionalOperations
  "Protocol to allow functional-style operations on matrix elements."
  ;; note that protocols don't like variadic args, so we convert to regular args
  ;; also the matrix type must be first for protocol dispatch, so we move it before f
  (element-seq [m])
  (element-map [m f]
               [m f a]
               [m f a more])
  (element-map! [m f]
                [m f a]
                [m f a more])
  (element-reduce [m f] [m f init]))
