(ns clojure.core.matrix.protocols
  "Namespace for core.matrix protocols. These protocols are intended to be implemented by
   core.matrix array implementations.

   Note to implementers:
    - Please read the docstrings for the protocols you are implementing!
    - Protocols should be implemented correctly to achieve a compliant core.matrix implementations

   core.matrix users should normally avoid using this namespace directly
   and instead use the functions in the main clojure.core.matrix API"
  (:require [clojure.core.matrix.utils :refer [same-shape-object?]]
            [clojure.core.matrix.impl.mathsops :as mops])
  (:refer-clojure :exclude [clone])
  (#?(:clj :require :cljs :require-macros)
           [clojure.core.matrix.macros :refer [error]])
  #?(:clj (:import [java.util List]
                   [clojure.lang Seqable])))

#?(:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
))

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
    "Returns a keyword representing this implementation, that can be used to request array instances or
     look up implementation metadata etc.

     Each implementation should have one unique key. Official mapping of implementation keywords is
     maintained in the var clojure.core.matrix.implementations/KNOWN-IMPLEMENTATIONS.")
  (meta-info [m]
    "Returns optional meta-information on the implementation.

     Standard keys:
       :doc - containing a string describing an implementation")
  (construct-matrix [m data]
    "Returns a new n-dimensional array containing the given data. data should be in the form of either
     nested sequences or a valid existing array.

     The return value should be in the preferred format of the given implementation. If the implementation
     does not support the required dimensionality or element type then it may either:
      - Throw an error
      - Return nil to indicate that a default implementation should be used instead

     0-dimensional arrays / scalars are permitted.")
  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length, filled with numeric zero.")
  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and columns, filled with numeric zero.")
  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Must return nil if the shape is not supported by the implementation.
     Shape can be any sequence of integer dimension sizes (including 0 dimensions).")
  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number of dimensions."))

(defprotocol PDimensionInfo
  "Protocol to return standard dimension information about an array.
   dimensionality and dimension-count are mandatory for implementations"
  (dimensionality [m]
    "Returns the number of dimensions of an array, as an integer (greater than or equal to zero). ")
  (get-shape [m]
    "Returns the shape of the array, typically as a Java array or sequence of dimension sizes.
     Implementations are free to choose what type is used to represent the shape, but it must
     contain only integer values and be traversable as a sequence via clojure.core/seq")
  (is-scalar? [m]
    "Tests whether an object is a scalar value, i.e. a value that can exist at a
     specific position in an array.")
  (is-vector? [m]
    "Tests whether an object is a vector (1D array)")
  (dimension-count [m dimension-number]
    "Returns the size of a specific dimension. Must throw an exception if the array does not
     have the specified dimension."))

;; protocol arity overloads behave oddly, so different names used for simplicity
;; we provide fast paths for 1D and 2D access (common case)
(defprotocol PIndexedAccess
  "Protocol for indexed read access to arrays, matrices and vectors. Indexing values can
   be assumed to be integers. indexes may be any sequence or Java array of index values."
  (get-1d [m row])
  (get-2d [m row column])
  (get-nd [m indexes]))

(defprotocol PIndexedSetting
  "Protocol for indexed 'setter' operations. These are like Clojure's 'assoc'
   function, i.e. they return an updated copy of the original array, which is itself unchanged.
   Should be supported for any immutable array type."
  (set-1d [m row v])
  (set-2d [m row column v])
  (set-nd [m indexes v])
  (is-mutable? [m]
    "Returns true if the matrix is mutable and therefore supports direct mutable operations, e.g. add!"))

;; ===================================================================================
;; MANDATORY PROTOCOLS FOR MUTABLE MATRICES
;;
;; A compliant clojure.core.matrix mutable implementation must implement these.
;; Otherwise things will fail.

(defprotocol PIndexedSettingMutable
  "Protocol for indexed mutable setter access to matrices and vectors.
   Must be supported for any mutable matrix type."
  (set-1d! [m row v])
  (set-2d! [m row column v])
  (set-nd! [m indexes v]))

(defprotocol PMatrixCloning
  "Protocol for cloning a matrix value. The new clone must be mutable if the original
   matrix is mutable, i.e. mutating the clone must not affect the original. The copy should be shallow, if applicable."
  (clone [m] "Returns a clone of an array. Must be a new independent (non-view)
              instance if the array is mutable."))

;; ===================================================================================
;; RECOMMENDATION: Implement Sequable or Iterable
;;
;; It is strongly recommended that core.matrix implementations implement either the
;; clojure.lang.Seqable or java.lang.Iterable interface.
;;
;; The semantics should be to return a seq / iterator of row-major slices
;;
;; This enables matrices to be used with regular Clojure sequence operations e.g.
;;
;; (first (matrix [[1 2] [3 4])) => (matrix [1 2])
;; (map #(add % [1 2]) [[1 2] [3 4] [5 6]]) => ([2 4] [4 6] [6 8])
;;
;; Unfortunately this recommendation cannot be enforced, since it is impossible to
;; retrofit old Java classes with new interface implementations :-(

;; ===================================================================================
;; OPTIONAL PROTOCOLS
;;
;; implementations don't need to provide these since fallback default implementations
;; are provided. However, they should consider doing so for performance reasons

(defprotocol PTypeInfo
  "Protocol for querying the type of matrix elements. If not provided, the default implementation will
   return java.lang.Object, and the matrix object is assumed to accept any type of value.
   If a matrix is primitive-backed, it should return the appropriate primitive type e.g. Double/TYPE."
  (element-type [m]))

(defprotocol PArrayMetrics
  "Optional protocol for quick determination of array matrics"
  (nonzero-count [m]
    "Returns the number of non-zero elements in a numerical array. 
     May throw an exception if the array is not numerical."))

(defprotocol PValidateShape
  "Optional protocol to validate the shape of a matrix. If the matrix has an incorrect shape, should
   throw an error. Otherwise it should return the correct shape."
  (validate-shape
    [m] 
    [m expected-shape]
    "Returns the shape of the array, performing validation and throwing an error if the shape is inconsistent"))

(defprotocol PRowColMatrix
  "Protocol to support construction of row and column matrices from 1D vectors.

   A vector of length N should be converted to a 1xN or Nx1 matrix respectively.

   Should throw an error if the data is not a 1D vector"
  (column-matrix [m data])
  (row-matrix [m data]))

(defprotocol PMutableMatrixConstruction
  "Protocol for creating a mutable copy of a matrix. If implemented, must return either a fully mutable
   copy of the given matrix, or nil if not possible.

   The default implementation will attempt to choose a suitable mutable matrix implementation."
  (mutable-matrix [m]))

(defprotocol PMutableCoercion
  "Protocol for coercing to a mutable format. May return the original array, if it is already fully mutable,
   otherwise should return a fully mutable copy of the array.

   Should return nil to indicate that this implementation cannot create a mutable array from the given data.

   The default implementation will attempt to choose a suitable mutable matrix implementation."
  (ensure-mutable [m] "Returns this array if fully mutable, otherwise returns a new mutable array containing
                   a copy of this array. May return nil if the implementation cannot create a suitable mutable
                   array."))

(defprotocol PSparse
  "Protocol for constructing a sparse array from the given data. Implementations should
   consider the possibility that data may be a large lazy sequence, possibly larger than memory, so should ideally
   attempt to construct the sparse matrix incrementally without realising the whole sequence at once.

   May return nil if no sparse conversion is available."
  (sparse-coerce [m data] "Attempts to coerce data to a sparse array of implementation m. May return nil if not supported")
  (sparse [m] "Attempts to make array into a sparse format. Must return the same array unchanged if not possible."))

(defprotocol PNative
  "Protocol for creating and handling native arrays. Implementations must return a native format array if they
   are able to, or nil otherwise."
  (native [m] "Attempts to coerce data to a native array of implementation m. May return nil if not supported")
  (native? [m] "Returns true if an array is in a native format, false otherwise."))

(defprotocol PDense
  "Protocol for constructing a dense array from the given data."
  (dense-coerce [m data] "Attempts to coerce data to a dense array of implementation m. May return nil if not supported")
  (dense [m] "Attempts to make array into a dense format. Must return the same array unchanged if not possible."))

(defprotocol PImmutableMatrixConstruction
  "Protocol for creating an immutable copy of a matrix. If implemented, must return a fully immutable
   copy of the given matrix.

   The default implementation will attempt to choose a suitable immutable matrix implementation."
  (immutable-matrix [m]))

(defprotocol PZeroDimensionConstruction
  (new-scalar-array
    [m]
    [m value]
    "Construct a new zero-dimensional array with the specified scalar value (zero if not specified)"))

(defprotocol PZeroDimensionAccess
  "Protocol for accessing the scalar value in zero-dimensional arrays. Zero dimensional arrays differ
   from scalar values in the following two senses:
    - They may be mutable (in which case set-0d! is expected to work)
    - They are not considered themselves to be scalars. Hence you must use get-0d to access the
      contained scalar value"
  (get-0d
    [m]
    "Gets the scalar value in an 0d array.")
  (set-0d!
    [m value]
    "Sets the scalar value in the 0d array to a given value. Throws an error if not mutable."))

(defprotocol PZeroDimensionSet
  "Protocol for setting the scalar value in zero-dimensional arrays."
  (set-0d [m value] "Sets the scalar value in a 0-d array, returning a new 0-d array"))

(defprotocol PSpecialisedConstructors
  "Protocol for construction of special matrices."
  (identity-matrix
    [m dims]
    "Create a 2D identity matrix with the given number of dimensions")
  (diagonal-matrix
    [m diagonal-values]
    "Create a diagonal matrix with the specified leading diagonal values"))

(defprotocol PPermutationMatrix
  "Protocol for construction of a permutation matrix."
  (permutation-matrix [m permutation]))

(defprotocol PBlockDiagonalMatrix
  "Protocol for construction of a block diagonal matrix."
  (block-diagonal-matrix [m blocks]))

(defprotocol PCoercion
  "Protocol to coerce a parameter to a format used by a specific implementation. It is
   up to the implementation to determine what parameter types they support.
   If the implementation is unable to perform coercion, it must return nil.

   Implementations are encouraged to avoid taking a full copy of the data, for performance reasons.
   It is preferable to use structural sharing with the original data if possible.

   If coercion is impossible (e.g. param has an invalid shape or element types) then the
   implementation *may* throw an exception, though it may also return nil to get default behaviour,
   which should implement any expected exceptions.

   If an implementation implements coercion via copying, then it is recommended that conversion
   should be to the most efficient packed representation (i.e. as defined by 'pack')

   Implementations must also be able to coerce valid scalar values (presumably via the identity function)"
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
      - not included in the array (i.e. the target shape has more leading dimensions)

     If broadcasting is not possible, an exception must be thrown.

     Broadcasting may return either a view with replicated element or a new immutable matrix."))

(defprotocol PBroadcastLike
  "Protocol to broadcast into a given matrix shape. May also perform coercion if needed by the implementation."
  (broadcast-like [m a]))

(defprotocol PBroadcastCoerce
  "Protocol to broadcast into a given matrix shape and perform coercion in one step.

   Equivalent to (coerce m (broadcast-like m a)) but likely to be more efficient."
  (broadcast-coerce [m a] "Broadcasts and coerces a to the same shape and implementation as m"))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers,
   however providing an efficient implementation is strongly encouraged to enable fast interop
   with Clojure vectors."
  (convert-to-nested-vectors [m] "Converts an array to nested Clojure persistent vectors"))

(defprotocol PReshaping
  "Protocol to reshape matrices. Should support any new shape allowed by the implementation.
   Must preserve row-major ordering of matrix elements.
   If the original matrix is mutable, must return a new mutable copy of data.
   If the new shape has less elements than the original shape, it is OK to truncate the remaining elements.
   If the new shape requires more elements than the original shape, should throw an exception."
  (reshape [m shape]))

(defprotocol PReshapeView
  "Protocol to reshape matrices. Guarantees a view over the original data if mutable.
   If the new shape has less elements than the original shape, must truncate the remaining elements.
   Behaviour is undefined if the new shape requires more elements than the original shape."
  (reshape-view [m shape]))

(defprotocol PPack
  "Protocol to efficiently pack an array, according to the most efficient representation for a given
   implementation.

   Definition of pack is up to the implementation to interpret, but the general rules are:
   1. Must not change the value of the array for comparison purposes
   2. Must not change the shape of the array
   3. May preserve sparse representation
   4. Should convert to most efficient format for common operations (e.g. mget, inner-product)"
  (pack [m]))

(defprotocol PSameShape
  "Protocol to test if two arrays have the same shape. Implementations may have an optimised
   method for shape equality tests, and this is a frequently required operations so it may
   make sense to provide an optimised implementation."
  (same-shape? [a b]))

(defprotocol PMatrixSlices
  "Protocol to support getting slices of an array.  If implemented, must return either a view, a scalar
   or an immutable sub-matrix: it must *not* return copied data. i.e. making a full copy must be avoided."
  (get-row [m i]
    "Gets a row of a matrix with the given row index.")
  (get-column [m i]
    "Gets a column of a matrix with the given row index.")
  (get-major-slice [m i]
    "Gets the major slice of an array with the given index. For a 2D matrix, equivalent to get-row")
  (get-slice [m dimension i]
    "Gets a slice of an array along a specified dimension with the given index."))

(defprotocol PMatrixRows
  "Protocol for accessing rows of a matrix"
  (get-rows [m] "Returns the rows of a matrix, as a seqable object"))

(defprotocol PMatrixColumns
  "Protocol for accessing columns of a matrix"
  (get-columns [m] "Returns the columns of a matrix, as a seqable object"))

(defprotocol PSliceView
  "Protocol for quick view access into a row-major slices of an array. If implemented, must return
   either a view or an immutable sub-matrix: it must *not* return copied data.

   If the matrix is mutable, it must return a mutable view.

   The default implementation creates a wrapper view."
  (get-major-slice-view [m i] "Gets a view of a major array slice"))

(defprotocol PSliceView2
  "Protocol for quick view access into a slices of an array. If implemented, must return
   either a view or an immutable sub-matrix: it must *not* return copied data.

   If the matrix is mutable, it must return a mutable view.

   The default implementation creates a wrapper view."
  (get-slice-view [m dim i] "Gets a view of an array slice along the specified dimension."))

(defprotocol PSliceSeq
  "Returns the row-major slices of the array as a sequence.

   These must be views or immutable sub-arrays for higher order slices, or scalars
   for the slices of a 1D vector.

   The default implementation uses get-major-slice-view to obtain the slices."
  (get-major-slice-seq [m]
    "Gets a sequence of all major array slices"))

(defprotocol PSliceSeq2
  "Returns slices of the array as a sequence.

   These must be views or immutable sub-arrays for higher order slices, or scalars
   for the slices of a 1D vector."
  (get-slice-seq [m dim]
    "Gets a sequence of all array slices"))

(defprotocol PSliceViewSeq
  "Returns the row-major slice views of the array.

   These must be arrays if the array is mutable, i.e. slices of a 1D vector
   must be 0-dimensional mutable arrays."
  (get-major-slice-view-seq [m] "Gets a sequence of all major array slices"))

(defprotocol PSliceJoin
  "Protocol for concatenating / joining arrays."
  (join [m a] "Concatenates a to m, along the major slice dimension"))

(defprotocol PSliceJoinAlong
  "Protocol for concatenating / joining arrays."
  (join-along [m a dim] "Concatenates a to m, along the slice dimension dim"))

(defprotocol PSubVector
  "Protocol for getting a sub-vector view of a vector. Must return a mutable view
   if the original vector is mutable. Should throw an exception if the specified
   subvector is out of bounds for the target vector."
  (subvector [m start length]
    "Gets a sub-vector of a vector. Must return a view if the vector is mutable."))

;; TODO: should return either an immutable sub-matrix or a mutable view
(defprotocol PMatrixSubComponents
  "Protocol for picking out subsections of a 2D matrix. Should return a mutable view if possible.
   The default implementation creates a new vector containing the diagonal values."
  (main-diagonal [m]
    "Returns the main (leading) diagonal of a matrix."))

(defprotocol PSparseArray
  "Protocol for determining if an array is in a sparse format. It is up to the implementation to define
   its own sparse formats, but in general the intention should be that a sparse array uses significantly
   less storage than an equivalent dense array, assuming a high proportion of zero values in the array."
  (is-sparse? [m]
    "Returns true if the array is in a sparse format, as defined by the implementation."))

(defprotocol PNewSparseArray
  "Protocol for constructing sparse arrays. Should return nil if the sparse array shape is not supported."
  (new-sparse-array
    [m shape]
    "Creates a new sparse array with the given shape."))

(defprotocol PZeroCount
  "Protocol for counting the number of zeros in a numerical array. Must return an integer value
   representing the precise number of zeros."
  (zero-count
    [m]
    "Returns the number of zeros in the array"))

;; ==========================================================================
;; Array assignment and conversion operations

(defprotocol PAssignment
  "Protocol for assigning values element-wise to mutable arrays."
  (assign!
    [m source]
    "Sets all the values in an array from a given source. Source may be a scalar
     or any smaller array that can be broadcast to the shape of m.")
  (assign-array!
    [m arr]
    [m arr start length]
    "Sets the elements in an array from a Java array source, in row-major order."))

(defprotocol PImmutableAssignment
  "Protocol for assigning values element-wise to an array, broadcasting as needed."
  (assign
    [m source]
    "Sets all the values in an array from a given source. Source may be a scalar
     or a smaller array that can be broadcast to the shape of m."))

(defprotocol PMutableFill
  (fill!
    [m value]
    "Fills the array with the given scalar value."))

(defprotocol PDoubleArrayOutput
  "Protocol for getting element data as a flattened double array"
  (to-double-array
    [m]
    "Returns a new double array containing the values of m in row-major order. May or may not be
     the internal double array used by m, depending on the implementation, but if it is the internal array
     this should be the same array returned by 'as-double-array'. This should in general be the most
     efficient way of getting a double array.")
  (as-double-array
    [m]
    "Returns the internal double array used by m. If no such array is used, returns nil.
     Provides an opportunity to avoid copying the internal array."))

(defprotocol PObjectArrayOutput
  "Protocol for getting element data as a flattened object array"
  (to-object-array
    [m]
    "Returns an object array containing the values of m in row-major order. May or may not be
     the internal object array used by m, depending on the implementation.")
  (as-object-array
    [m]
    "Returns the internal object array used by m. If no such array is used, returns nil.
     Provides an opportunity to avoid copying the internal array."))

;; ============================================================
;; Equality operations
;;
;; Should be implemented for efficient performance on matrix equality checks

(defprotocol PValueEquality
  "Protocol for comparing two arrays, with the semantics of clojure.core/=.
   Must return false if the arrays are not of equal shape, or if any elements are not equal."
  (value-equals
    [m a]
    "Returns true if two arrays are equal both in shape and according to clojure.core/= for each element."))

(defprotocol PMatrixEquality
  "Protocol for numerical array equality operations."
  (matrix-equals
    [a b]
    "Return true if a equals b, i.e. if a and b have the same shape and all elements are equal.
     Must use numerical value comparison on numbers (==) to account for matrices that may hold a mix of
     numercial types (e.g. java.lang.Long and java.lang.Double). Implementations that only support doubles
     should use Number.doubleValue() to get a numeric value to compare.
     May throw an exception if the matrices are non-numeric"))

(defprotocol PMatrixEqualityEpsilon
  "Protocol for numerical array equality operations with a specified tolerance. Arrays are defined as equal
   if the array shapes are the same and and for all corresponding elements ai and bi we have: |ai-bi|<=eps

   Should be equivalent to PMatrixEquality when eps is zero."
  (matrix-equals-epsilon
    [a b eps]
    "As matrix-equals, but provides a numerical tolerance for equality testing."))

;; ============================================================
;; Mathematical operations
;;
;; These protocols are generally optional but should be implemented for
;; optimised performance on the given operations.

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on numerical arrays.

   Implementation may return nil if the implementation does not support one of the parameters, in
   which case a more general operation will be attempted."
  (matrix-multiply [m a])
  (element-multiply [m a]))

(defprotocol PMatrixProducts
  "Protocol for general inner and outer products of numerical arrays.
   Products should use + and * as normally defined for numerical types."
  (inner-product [m a] "Returns the inner product of two numerical arrays.")
  (outer-product [m a] "Returns the outer product of two numerical arrays. Implementation
                        may return nil to indicate that a default computation should be used."))

(defprotocol PAddProduct
  "Optional protocol for add-product operation.

   Intended to support optimised implementations for result = m + a * b"
  (add-product [m a b]))

(defprotocol PAddProductMutable
  "Optional protocol for mutable add-product! operation.

   Intended to support optimised implementations for m = m + a * b"
  (add-product!
    [m a b] "Adds the elementwise product of a and b to m"))

(defprotocol PAddScaledProduct
  "Protocol for add-product operation.

   Intended to support optimised implementations for result = m + a * b * factor"
  (add-scaled-product
    [m a b factor] "Adds the elementwise product of a, b and a scalar factor to m"))

(defprotocol PAddScaledProductMutable
  "Protocol for mutable add-product! operation.

   Intended to support optimised implementations for m = m + a * b * factor"
  (add-scaled-product! [m a b factor]))

(defprotocol PAddScaled
  "Protocol for add-scaled operation.

   Implementations may assume that factor is a scalar.

   Intended to support optimised implementations for result = m + a * factor"
  (add-scaled [m a factor]))

(defprotocol PAddScaledMutable
  "Protocol for mutable add-scaled! operation.

   Implementations may assume that factor is a scalar.

   Intended to support optimised implementations for m = m + a * factor"
  (add-scaled! [m a factor]))

(defprotocol PMatrixDivide
  "Protocol to support element-wise division operator.

   One-arg version returns the reciprocal of all elements."
  (element-divide
    [m]
    [m a]))

(defprotocol PMatrixDivideMutable
  "Protocol to support mutable element-wise division operater.

   One-arg version computes the reciprocal of all elements."
  (element-divide!
    [m]
    [m a]))

(defprotocol PMatrixMultiplyMutable
  "Protocol to support mutable matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply! [m a])
  (element-multiply! [m a]))

(defprotocol PVectorTransform
  "Protocol to support transformation of a vector to another vector. Is equivalent to matrix multiplication
   when 2D matrices are used as transformations. But other transformations are possible, e.g. non-affine
   transformations.

   A transformation need not be a core.matrix matrix: other types are permissible"
  (vector-transform [t v] "Transforms a vector")
  (vector-transform! [t v] "Transforms a vector in place - mutates the vector argument"))

(defprotocol PMatrixScaling
  "Protocol to support numerical array scaling by scalar values. Provided because array classes may have
   efficient specialised scaling operaions.

   Works according the the default definition of multiplication for the matrix class
   (usually numerical, i.e. equivalent to clojure.core/+)"
  (scale [m constant]
    "Multiplies a array by the scalar constant, ")
  (pre-scale [m constant]
    "Pre-multiplies the array with the scalar constant. This is the same as scale for arrays
     where multiplication is commutative, but may be different for special kinds of scalars."))

(defprotocol PMatrixMutableScaling
  "Protocol to support mutable array scaling by scalar values."
  (scale! [m factor])
  (pre-scale! [m factor]))

(defprotocol PMatrixAdd
  "Protocol to support addition and subtraction on arbitrary matrices.
   These are elementwise operations that should support broadcasting."
  (matrix-add [m a])
  (matrix-sub [m a]))

(defprotocol PMatrixAddMutable
  "Protocol to support mutable addition and subtraction"
  (matrix-add! [m a])
  (matrix-sub! [m a]))

(defprotocol PScaleAdd
  "Protocol to support the mutable scale-add! operation. This is a common operation that may be
   optimised by the underlying implementation. Implementations should consider extra optimisations for
   specific constant values e.g. 0.0 and 1.0 but this is not mandatory."
  (scale-add! [m1 a m2 b constant]
    "Scales array m1 in place by factor b, then adds array m2 scaled by factor b, then adds the constant"))

(defprotocol PScaleAdd2
  "Protocol to support the immutable scale-add! operation."
  (scale-add [m1 a m2 b constant]
    "Scales array m1 by factor b, then adds array m2 scaled by factor b, then adds the constant"))

(defprotocol PLerp
  "Protocol to support the lerp linear interpolation function."
  (lerp [a b factor]
    "Linear interpolation: Scales array a by (1-factor), then adds array b scaled by factor.")
  (lerp! [a b factor]
    "Linear interpolation: Scales array a by (1-factor), then adds array b scaled by factor. Mutates a."))

(defprotocol PAddInnerProductMutable
  "Protocol to support the mutable add-inner-product! operation. This is a common operation that may be
   optimised by the underlying implementation. Implementations should consider extra optimisations for
   specific constant factors e.g. 0.0 and 1.0 but this is not mandatory."
  (add-inner-product!
    [m a b]
    [m a b factor]
    "Adds the inner product of a, b and an optional scalar factor to m"))

(defprotocol PAddOuterProductMutable
  "Protocol to support the mutable add-outer-product! operation. This is a common operation that may be
   optimised by the underlying implementation. Implementations should consider extra optimisations for
   specific constant factors e.g. 0.0 and 1.0 but this is not mandatory."
  (add-outer-product!
    [m a b]
    [m a b factor]
    "Adds the outer product of a, b and an optional scalar factor to m"))

(defprotocol PSetInnerProductMutable
  "Protocol to support the mutable set-inner-product! operation. This is a common operation that may be
   optimised by the underlying implementation. Implementations should consider extra optimisations for
   specific constant factors e.g. 0.0 and 1.0 but this is not mandatory."
  (set-inner-product!
    [m a b]
    [m a b factor]
    "Sets m to the inner product of a, b and an optional scalar factor to m"))

(defprotocol PSubMatrix
  "Protocol to get a subarray of another array. dim-ranges should be a sequence of [start len]
   pairs, one for each dimension. If a pair is nil, it should be interpreted to take the whole dimension.

   Returning a mutable view is preferred, if the implementation supports this."
  (submatrix [d dim-ranges]))

(defprotocol PComputeMatrix
  "Protocol to compute a matrix by calling a function on each indexed location. The function f will be called
   as (f x y z ...) for all index values."
  (compute-matrix [m shape f]))

(defprotocol PTranspose
  "Protocol for array transpose operation"
  (transpose [m]
    "Returns the transpose of an array. Equivalent to reversing the \"shape\".
     Note that:
     - The transpose of a scalar is the same scalar
     - The transpose of a 1D vector is the same 1D vector
     - The transpose of a 2D matrix swaps rows and columns"))

(defprotocol PTransposeDims
  "Protocol for generalised array transpose operation"
  (transpose-dims [m order]
    "Returns the transpose of an array, reordering the dimensions in the specified order."))

(defprotocol PRotate
  "Rotates an array along a specified dimension by the given number of places.

   Rotating a dimension that does not exist has no effect on the array."
  (rotate [m dim places]))

(defprotocol PRotateAll
  "Rotates an array using the specified shifts for each dimension.

   shifts may be any sequence of integer shift amounts."
  (rotate-all [m shifts]))

(defprotocol PShift
  "Rotates an array using the specified shifts for each dimension. Newly shifted in elements
   should be filled with the default scalar value (usually zero)."
  (shift [m dim places]
    "Shift along a single specified dimension")
  (shift-all [m shifts]
    "Shift along all specified dimensions as a single operation.

     `shifts` may be any sequence of integer shift amounts."))

(defprotocol PTransposeInPlace
  "Protocol for mutable 2D matrix transpose in place"
  (transpose! [m]
    "Transposes a mutable 2D matrix in place"))

(defprotocol POrder
  "Protocol for matrix reorder.

   By default, re-orders along the first (major) dimension, but may reorder along any dimension by
   specifiying the dimension argument.

   Indicies can be any seqable object containing the indices along the specified dimension to select.
   An index can be selected multiple times (which created repreated slices), or not at all (which excludes
   the slice from the result).

   Some implementation may implement re-ordering using lightweight or mutable views over the original array
   data."
  (order
    [m indices]
    [m dimension indices]))

(defprotocol PNumerical
  "Protocol for identifying numerical arrays. Should return true if every element in the
   array is guaranteed to be a valid numerical value."
  (numerical? [m]
    "Returns true if the array is numerical."))

(defprotocol PVectorOps
  "Protocol to support common numerical vector operations."
  (vector-dot [a b]
     "Numerical dot product of two vectors. Must return a scalar value if the two parameters are
      vectors of equal length.

      If the vectors are of unequal length, should throw an exception (however returning nil is
      also acceptable).

      Otherwise the implementation may optionally either return nil or compute a higher dimensional
      inner-product (if it is able to do so).")
  (length [a]
     "Euclidian length of a vector.")
  (length-squared [a]
     "Squared Euclidean length of a vector.")
  (normalise [a]
     "Returns a new vector, normalised to length 1.0"))

(defprotocol PVectorCross
  (cross-product [a b]
    "Cross product of two vectors")
  (cross-product! [a b]
    "Calculate cross product of two vectors, storing the result in the first vector"))

(defprotocol PVectorDistance
  (distance [a b]
     "Euclidean distance of two vectors."))

(defprotocol PVectorView
  (as-vector [m]
    "Returns a view of an array as a single flattened vector. May return the vector itself
     if it is already a 1D vector."))

(defprotocol PVectorisable
  "Protocol to return an array as a flattened vector of all elements.
   Implementations are encouraged to avoid taking a full copy of all data
   (e.g. by using structural sharing or views)."
  (to-vector [m]
    "Returns an array as a single flattened vector"))


(defprotocol PMutableVectorOps
  "Protocol for mutable versions of common vector operations"
  (normalise! [a]))

(defprotocol PMatrixOps
  "Protocol to support common 2D numerical matrix operations"
  (trace [m]
    "Returns the trace of a matrix (sum of elements on main diagonal.
     Must throw an error if the matrix is not square (i.e. different number of rows and columns)")
  (determinant [m]
    "Returns the determinant of a matrix. May return nil if the implementation is unable
     to compute determinants, in which case a default implementation will be tried.
     Must throw an error if the matrix is not square (i.e. different number of rows and columns)")
  (inverse [m]
    "Returns the inverse of a matrix. Should return nil if m is not invertible."))

(defprotocol PNegation
  (negate [m]
    "Returns a new numerical array with all elements negated."))

(defprotocol PMatrixRank
  "Protocol to support computing the rank (number of linearly independent rows) in a matrix"
  (rank [m]
        "Returns the rank of a matrix"))

(defprotocol PIndexRank
  "Protocol to support ranking of elements in an array."
  (index-rank 
    [m]
    [m comparator] 
    "Returns an array of indexed ranks, using an optional comparator"))

(defprotocol PSummable
  "Protocol to support the summing of all elements in an array.
   The array must hold numeric values only, or an exception will be thrown."
  (element-sum [m]))

(defprotocol PExponent
  "Protocol to support the 'pow' function. Should raise every element of a matrix to a
   given exponent. Default implementation uses Java's Math/pow function which is appropriate for
   double values: arrays supporting arbitrary precision numbers or complex types will need to
   provide their own implementation."
  (element-pow [m exponent]))

(defprotocol PSquare
  "Protocol to support element-wise squaring of a numerical array."
  (square [m]))

(defprotocol PLogistic
  "Protocol to support element-wise logistic function on a numerical array."
  (logistic [m]))

(defprotocol PLogisticMutable
  "Protocol to support mutable element-wise logistic function on a numerical array."
  (logistic! [m]))

(defprotocol PSoftplus
  "Protocol to support element-wise softplus function on a numerical array."
  (softplus [m]))

(defprotocol PSoftplusMutable
  "Protocol to support mutable element-wise softplus function on a numerical array."
  (softplus! [m]))

(defprotocol PReLU
  "Protocol to support element-wise relu function on a numerical array."
  (relu [m]))

(defprotocol PReLUMutable
  "Protocol to support mutable element-wise relu function on a numerical array."
  (relu! [m]))

(defprotocol PSoftmax
  "Protocol to support element-wise softmax function on a numerical vector."
  (softmax [m]))

(defprotocol PSoftmaxMutable
  "Protocol to support mutable element-wise softmax function on a numerical vector."
  (softmax! [m]))

;; ==================================
;; Elementary Row Operation Protocols
;;

(defprotocol PRowOperations
  "Protocol for elementary row operations"
  (swap-rows [m i j]
    "Returns a new matrix with rows i and j swapped")
  (multiply-row [m i k]
    "Returns a new matrix with row i multiplied by k")
  (add-row [m i j k]
    "Returns a new matrix with row i added to row j times k"))

(defprotocol PRowSetting
  "Protocol for row setting. Should set a dimension 0 (row) slice to thegiven row value."
  (set-row [m i row])
  (set-row! [m i row]))

(defprotocol PColumnSetting
  "Protocol for column setting. Should set a dimension 1 (column) slice to the given column value."
  (set-column [m i column])
  (set-column! [m i column]))

(defprotocol PMathsFunctions
  "Protocol to support mathematical functions applied element-wise to a numerical array."
  (abs [m])
  (acos [m])
  (asin [m])
  (atan [m])
  (cbrt [m])
  (ceil [m])
  (cos [m])
  (cosh [m])
  (exp [m])
  (floor [m])
  (log [m])
  (log10 [m])
  (round [m])
  (signum [m])
  (sin [m])
  (sinh [m])
  (sqrt [m])
  (tan [m])
  (tanh [m])
  (to-degrees [m])
  (to-radians [m]))

(defprotocol PMathsFunctionsMutable
  "Protocol to support mutable mathematical functions applied element-wise to a numerical array."
  (abs! [m])
  (acos! [m])
  (asin! [m])
  (atan! [m])
  (cbrt! [m])
  (ceil! [m])
  (cos! [m])
  (cosh! [m])
  (exp! [m])
  (floor! [m])
  (log! [m])
  (log10! [m])
  (round! [m])
  (signum! [m])
  (sin! [m])
  (sinh! [m])
  (sqrt! [m])
  (tan! [m])
  (tanh! [m])
  (to-degrees! [m])
  (to-radians! [m]))

(defprotocol PElementCount
  "Protocol to return the total count of elements in matrix. Result may be any integer type,
   typically a java.lang.Long"
  (element-count [m]))

(defprotocol PElementMinMax
  "Protocol to return the minimum and maximum elements in a numerical array. Must throw an exception
   if the array is not numerical."
  (element-min [m])
  (element-max [m])
  (element-clamp [m a b]
    "Returns a matrix where the elements are clamped to be within lower and
    upper bounds specified by a and b, respectively."))

(defprotocol PCompare
  "Protocol to allow element-wise comparison of elements in an array or matrix."
  (element-compare [a b]
    "Return the sign (signum) of the element-wise substraction of two scalars,
    arrays or matrices i.e., must satisfy (signum (sub A B).")
  (element-if [m a b]
    "Element-wise if statement.

    Traverse each element, x, of a array or matrix, m. If:
      - x > 0, return a (if scalar) or corresponding element of a (if a is an
        array or matrix with same shape shape as m).
      - x <= 0, return b (if scalar) or corresponding element in b (if b is an
        array or matrix with same shape shape as m).

    Return an array or matrix with the same shape as m.")
  (element-lt [m a]
    "Return a binary array or matrix where elements of m less-than a are
    represented by 1 and elements greater-than a are represented as 0.")
  (element-le [m a]
    "Return a binary array or matrix where elements of m less-than-or-equal
    to a are  represented by 1 and elements greater-than a are represented as 0.")
  (element-gt [m a]
    "Return a binary array or matrix where elements of m greater-than a are
    represented by 1 and elements less-than a are represented as 0.")
  (element-ge [m a]
    "Return a binary array or matrix where elements of m greater-than-or-equal
    to a are  represented by 1 and elements less than a are represented as 0.")
  (element-ne [m a]
    "Return a binary array or matrix where elements of m not-equal to a are
    represented by 1 and elements equal to a are represented as 0.")
  (element-eq [m a]
    "Return a binary array or matrix where elements of m equal to a are
    represented by 1 and elements not-equal to a are represented as 0."))

(defprotocol PBLASBase
  "Base blas support.  Note that the largest differences
from the C blas functions is that the return value is provided
first so that the protocol machinery can work (as opposed to alpha, which
would often be a numeric base type)."
  (gemm! [c trans-a? trans-b? alpha a b beta])
  (gemv! [c trans-a? alpha a b beta]))

(defprotocol PSliceMap
  "Maps a function over every slice of one or more arrays."
  (slice-map
    [m f]
    [m f a]
    [m f a more]
    "Maps f over all slices of m (and optionally other arrays)"))

(defprotocol PFilterSlices
  "Filters the slices of the given array, returning only those which satisfy the given predicate."
  (filter-slices
    [m f]
    "Runs f on all slices of m. Must return those slices which satisfy (f slice).
     Must return nil if no slices meet the predicate.
     Must return either a new seqable array containing the filtered slices or a vector of slices
     (both of which are valid core.matrix arrays)"))

(defprotocol PFunctionalOperations
  "Protocol to allow functional-style operations on matrix elements."
  ;; note that protocols don't like variadic args, so we convert to regular args
  ;; also the matrix type must be first for protocol dispatch, so we move it before f
  (element-seq
    [m]
    "Must return a seqable object containing all elements of the matrix, in row-major order.
     i.e. it must be possible to call clojure.core/seq on the result. Valid sequable objects may
     include Java arrays, Clojure vectors/sequences, and any Java object that implement Iterable.")
  (element-map
    [m f]
    [m f a]
    [m f a more]
    "Maps f over all elements of m (and optionally other arrays), returning a new matrix.
     f is expected to produce elements of a type supported by the implementation of m - failure
     to do so may cause an error.")
  (element-map!
    [m f]
    [m f a]
    [m f a more]
    "Maps f over all elements of m (and optionally other arrays), mutating the elements of m in place.
     Must throw an exception if m is not mutable.
     f is expected to produce elements of a type supported by the implementation of m - failure
     to do so may cause an error.")
  (element-reduce
    [m f]
    [m f init]
    "Reduces with the function f over all elements of m.
     Implementations do not need to support clojure.core/reduced"))

(defprotocol PAddEmap
  "Protocol to support the add-emap! API function."
  (add-emap!
    [dest f a]
    [dest f a b]
    [dest f a b more]
    "Maps f over all elements of a (and optionally other arrays), adding the result to dest.
     Must throw an exception if dest is not mutable.
     f is expected to produce elements of a type supported by the implementation of dest - failure
     to do so may cause an error."))

(defprotocol PSetEmap
  "Protocol to support the set-emap! API function."
  (set-emap!
    [dest f a]
    [dest f a b]
    [dest f a b more]
    "Maps f over all elements of a (and optionally other arrays), storing the result in dest.
     Must throw an exception if dest is not mutable.
     f is expected to produce elements of a type supported by the implementation of dest - failure
     to do so may cause an error."))


(defprotocol PMapIndexed
  "Protocol for map-indexed operation on matrices"
  ;; This protocol is created instead of adding element-map-indexed to PFunctionalOperations to
  ;; avoid breaking backward compatibility of existing implementations
  (element-map-indexed
    [m f]
    [m f a]
    [m f a more]
    "Maps f over all elements of m (and optionally other matrices), returning a new matrix.
     f is expected to accept an index vector and the current element value, and produce
     elements of a type supported by the implementation of m - failure
     to do so may cause an error.")
  (element-map-indexed!
    [m f]
    [m f a]
    [m f a more]
    "Maps f over all elements of m (and optionally other matrices), mutating the elements of m in place.
     Must throw an exception if m is not mutable.
     f is expected to accept an index vector and the current element value, and produce
     elements of a type supported by the implementation of m - failure
     to do so may cause an error."))

(defprotocol PMatrixPredicates
  "Protocol for matrix predicates like identity-matrix? or zero-matrix?"
  (identity-matrix?
    [m]
    "Returns true if the matrix m is an identity matrix")
  (zero-matrix?
    [m]
    "Returns true if all the elements of matrix m are zeros")
  (symmetric?
    [m]
    "Returns true if matrix m is symmetric"))

(defprotocol PMatrixTypes
  (diagonal? [m] "Returns true if the matrix is diagonal, i.e. zero everywhere except the main diagonal")
  (upper-triangular? [m] "Returns true if the matrix m is upper triangualar")
  (lower-triangular? [m] "Returns true if the matrix m is lower triangualar")
  (positive-definite? [m] "Returns true if the matrix is positive definite")
  (positive-semidefinite? [m] "Returns true if the matrix is positive semidefinite")
  (orthogonal? [m eps] "Returns true if the matrix is orthogonal"))

;; ============================================================
;; Generic values and functions
;;
;; These protocols should be extended by implementations that use non-standard numerical operations
;; (e.g. complex numbers) and therefore require different values and operations for the standard
;; ones in Java / clojure.core

(defprotocol PGenericValues
  "Protocol for returning the generic/default values of a matrix implementation"
  (generic-zero [m] "Generic 'zero' value for numerical arrays. Must satisfy (equals m (add m zero)).")
  (generic-one [m] "Generic 'one' value for numerical arrays. Must satisfy (equals m (mul m one)).")
  (generic-value [m] "Generic value for a new array. Likely to be zero or nil."))

(defprotocol PGenericOperations
  "Protocol for returning the generic numerical functions of a matrix implementation"
  (generic-add [m] "Generic 'add' function for numerical values. Must satisfy (equals x (add zero x)).")
  (generic-mul [m] "Generic 'mul' function for numerical values. Must satisfy (equals x (mul one x)).")
  (generic-negate [m] "Generic 'negate' function for numerical values.")
  (generic-div [m] "Generic 'div' function for numerical values."))

;; ===========================================================
;; Protocols for higher-level array indexing

(defprotocol PSelect
  "Protocol for the sel function. See the docstring for clojure.core.matrix/select for
   more information on possible argument values."
  (select [a args] "selects all elements at indices which are in the cartesian product of args"))

(defprotocol PSelectView
  "Protocol for the sel function. Like PSelect, but guarantees an mutable view.

   If not supported by the implementation, may return nil to indicate that a default mutable view
   should be created."
  (select-view [a args] "selects all elements at indices which are in the cartesian product of args"))

(defprotocol PSetSelection
  "Protocol for setting the elements of an array returned by (select a args) to values.
   See the docstring for clojure.core.matrix/select for more information on possible argument values."
  (set-selection [a args values] "sets the elements in the selection of a to values"))

(defprotocol PIndicesAccess
  "Protocol for getting elements of an array at the specified indices."
  (get-indices [a indices] "returns a 1-d array with the elements of a at indices"))

(defprotocol PIndicesSetting
  "Protocol for setting elements of an array at the specified indices"
  (set-indices [a indices values] "sets the elements from a at indices to values. Returns a new array.")
  (set-indices! [a indices values] "destructively sets the elements from a at indices to values"))

(defprotocol PNonZeroIndices
  "Protocol for getting non-zero indices of an array"
   (non-zero-indices [m] "Gets the non-zero indices of an array.
                         - For a 1D vector, returns an ordered index list.
                         - For a higher dimensional array, returns the non-zero-indices for each slice in row-major order."))

(defprotocol PIndexImplementation
  "Protocol for determining if an object is a valid index. Implementations may implement this protocol to support their own index types."
  (index? [m] "Returns true if the argument is a valid index, false otherwise")
  (index-to-longs [m])
  (index-to-ints [m])
  (index-from-longs [m xs])
  (index-from-ints [m xs])
  (index-coerce [m a]))

;; ==========================================================
;; LABELLED DIMENSION PROTOCOLS

(defprotocol PDimensionLabels
  "Protocol for arrays supporting labelled dimensions"
  (label [m dim i] "Returns the label at a specific index along the given dimension")
  (labels [m dim] "Returns all labels along a given dimension, as a vector"))

(defprotocol PColumnNames
  "Protocol for arrays supporting labelled columns. This is a specialisation of label functionality
   intended for use by datasets, the key difference is that column-names should always select the
   last dimension."
  (column-name [m column] "Returns the label at a specific column")
  (column-names [m] "Returns all labels along the columns on an array"))

(defprotocol PColumnIndex
  "Protocol for getting the index of a named column. Works on any array with labelled columns.
   If the dimensionality is 1, assumes that columns are the only dimension (i.e. can be applied to
   dataset rows and Clojure maps in the natural way)
   Returns an integer index if the column is found, nil otherwise."
  (column-index [m column-label] "Returns the index of the specified column label"))

;; ==========================================================
;; LINEAR ALGEBRA PROTOCOLS

(defprotocol PNorm
  "Protocol for matrix and vector norms"
  (norm [m p]))

(defprotocol PQRDecomposition
  "Protocol for QR decomposition"
  (qr [m options]))

(defprotocol PCholeskyDecomposition
  "Procotol for Cholesky decomposition"
  (cholesky [m options]))

(defprotocol PLUDecomposition
  "Protocol for LU decomposition"
  (lu [m options]))

(defprotocol PSVDDecomposition
  "Protocol for SVD decomposition"
  (svd [m options]))

(defprotocol PEigenDecomposition
  "Procotol for Eigenvalue decomposition"
  (eigen [m options]))

(defprotocol PSolveLinear
  "Protocol for solving linear matrix equation or system of linear scalar equations"
  (solve [a b]))

(defprotocol PLeastSquares
  "Protocol for computing least-square solution to a linear matrix equation"
  (least-squares [a b]))

;; ============================================================
;; Dataset protocols

;; TODO: break up and use generic labelling functionality?
(defprotocol PDatasetImplementation
  "Protocol for general dataset functionality"
  (columns [ds] "Returns a persistent vector containing columns in the same order they are placed in the dataset")
  (select-columns [ds cols] "Produces a new dataset with the columns in the specified order")
  (select-rows [ds rows] "Produces a new dataset with specified rows")
  (add-column [ds col-name col] "Adds column to the dataset")
  (merge-datasets [ds1 ds2] "Returns a dataset created by combining columns of the given datasets. In case of columns with duplicate names, last-one-wins strategy is applied")
  (rename-columns [ds col-map] "Renames columns based on map of old new column name pairs")
  (replace-column [ds col-name vs] "Replaces column in a dataset with new values")
  (join-rows [ds1 ds2] "Returns a dataset created by combining the rows of the given datasets")
  (join-columns [ds1 ds2] "Returns a dataset created by combining the columns of the given datasets"))

(defprotocol PDatasetMaps
  (to-map [ds] "Returns map of columns with associated list of values")
  (row-maps [ds] "Returns seq of maps with row values"))

;; ============================================================
;; Utility functions
;;
;; Intended for use in protocol implementations

(defn persistent-vector-coerce
  "Coerces a data structure to nested persistent vectors"
  [x]
  (let [dims (long (dimensionality x))]
    (cond
      (== dims 0) (get-0d x) ;; first handle scalar / 0d case
      (clojure.core/vector? x) (mapv convert-to-nested-vectors x)
      (== dims 1) (vec (element-seq x))
      #?@(:clj [(instance? List x) (mapv convert-to-nested-vectors x)])
      (instance? #?(:clj Iterable :cljs IIterable) x) (mapv convert-to-nested-vectors x)
      (instance? #?(:clj Seqable :cljs ISeqable) x) (mapv convert-to-nested-vectors x)
      #?@(:clj [(.isArray (class x)) (mapv convert-to-nested-vectors (seq x))])
      (not (is-scalar? x)) (mapv convert-to-nested-vectors (get-major-slice-seq x))
      :default (error "Can't coerce to vector: " #?(:clj (class x)
                                                    :cljs (type x))))))

(defn- calc-common-shape
  "Returns the larger of two shapes if they are compatible, nil otherwise"
  ([a b]
    (let [ca (long (count a))
          cb (long (count b))
          diff (- ca cb)]
      (if (< diff 0)
        (recur b a)
        (loop [i 0]
          (if (< i cb)
            (if (== (nth a (+ diff i)) (nth b i))
              (recur (inc i))
              nil)
            a))))))

(defn common-shape
  "Returns the common shape that can be broadcast to from all the shapes specified,
   or nil if such a shape does not exist."
  ([shapes]
    (loop [result []
           shapes (seq shapes)]
      (if shapes
        (let [sh (first shapes)]
          (if-let [cs (calc-common-shape result sh)]
            (recur cs (next shapes))
            nil))
        result))))

(defn broadcast-compatible
  "Broadcasts two matrices into identical shapes, coercing to the type of the first matrix.
   Intended to prepare for elementwise operations.
   Returns a vector containing the two broadcasted matrices.
   Throws an error if not possible."
  ([a b]
    (if (same-shape? a b)
      [a b]
      (if (< (dimensionality a) (dimensionality b))
        [(broadcast-like b a) (coerce-param a b)]
        [a (broadcast-coerce a b)]))))

(defn broadcast-same-shape
  "Broadcasts two matrices into identical shapes. Intended to prepare for elementwise operations.
   Returns a vector containing the two broadcasted matrices.
   Throws an error if not possible."
  ([a b]
    (if (same-shape? a b)
      [a b]
      (let [sh (get-shape a)]
        (if (< (count sh) (dimensionality b))
          [(broadcast a (get-shape b)) b]
          [a (broadcast b sh)])))))

(defn same-shapes?
  "Returns truthy if a sequence of arrays all have the same shape."
  [arrays]
  (let [shapes (map #(or (get-shape %) []) arrays)]
    (loop [s (first shapes) ns (next shapes)]
      (if ns
        (if (same-shape-object? s (first ns))
          (recur s (next ns))
          false)
        true))))

(defn supports-type?
  "Checks if an array can contain a specified Java type."
  ([m ^Class klass]
    (let [^Class mc (element-type m)]
      (.isAssignableFrom mc klass))))

(defn ensure-type
  "Checks if an array can contain a specified Java type, if so returns the original array, otherwise
   returns a copy of the array that can support the specified type."
  [m ^Class klass]
  (if (supports-type? m klass)
    m
    (convert-to-nested-vectors m))) ;; TODO: better format?
