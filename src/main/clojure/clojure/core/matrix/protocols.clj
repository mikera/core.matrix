(ns clojure.core.matrix.protocols
  (:require [clojure.core.matrix.utils :refer [error same-shape-object? broadcast-shape]])
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
  (meta-info [m]
    "Returns meta-information on the implementation. It is expected that
     at least an element :doc containing a string describing an implementation
     is provided.")
  (construct-matrix [m data]
    "Returns a new matrix containing the given data. data should be in the form of either
     nested sequences or a valid existing matrix")
  (new-vector [m length]
    "Returns a new vector (1D column matrix) of the given length, filled with numeric zero.")
  (new-matrix [m rows columns]
    "Returns a new matrix (regular 2D matrix) with the given number of rows and columns, filled with numeric zero.")
  (new-matrix-nd [m shape]
    "Returns a new general matrix of the given shape.
     Must return nil if the shape is not supported by the implementation.
     Shape must be a sequence of dimension sizes.")
  (supports-dimensionality? [m dimensions]
    "Returns true if the implementation supports matrices with the given number of dimensions."))

(defprotocol PDimensionInfo
  "Protocol to return standard dimension information about an array.
   dimensionality and dimension-count are mandatory for implementations"
  (dimensionality [m]
    "Returns the number of dimensions of an array")
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
   Must be supported for any immutable array type."
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
   return java.lang.Object, and the matrix object must accept any type of value.
   If a matrix is primitive-backed, it should return the appropriate primitive type e.g. Double/TYPE."
  (element-type [m]))

(defprotocol PArrayMetrics
  "Option protocol for quick determination of array matrics"
  (nonzero-count [m]))

(defprotocol PValidateShape
  "Optional protocol to validate the shape of a matrix. If the matrix has an incorrect shape, should 
   throw an error. Otherwise it should return the correct shape."
  (validate-shape [m])) 

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

(defprotocol PSparse
  "Protocol for constructing a sparse array from the given data. Implementations should
   consider the possibility that data may be a large lazy sequence, possibly larger than memory, so should ideally
   attempt to construct the sparse matrix incrementally without realising the whole sequence at once.

   May return nil if no sparse conversion is available."
  (sparse-coerce [m data] "Attempts to coerce data to a sparse array of implementation m. May return nil if not supported")
  (sparse [m] "Attempts to make array into a sparse format. Must return the same array unchanged if not possible."))

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
  (get-0d [m])
  (set-0d! [m value]))

(defprotocol PZeroDimensionSet
  "Protocol for setting the scalar value in zero-dimensional arrays."
  (set-0d [m value] "Sets the scalar value in a 0-d array, returning a new 0-d array"))

(defprotocol PSpecialisedConstructors
  "Protocol for construction of special matrices."
  (identity-matrix [m dims] "Create a 2D identity matrix with the given number of dimensions")
  (diagonal-matrix [m diagonal-values] "Create a diagonal matrix with the specified leading diagonal values"))

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
  (broadcast-coerce [m a]))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers,
   however providing an efficient implementation is strongly encouraged to enable fast interop 
   with Clojure vectors."
  (convert-to-nested-vectors [m]))

(defprotocol PReshaping
  "Protocol to reshape matrices. Should support any new shape allowed by the implementation.
   Must preserve row-major ordering of matrix elements.
   If the original matrix is mutable, must return a new mutable copy of data.
   If the new shape has less elements than the original shape, it is OK to truncate the remaining elements.
   If the new shape requires more elements than the original shape, should throw an exception."
  (reshape [m shape]))

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
  (get-row [m i])
  (get-column [m i])
  (get-major-slice [m i])
  (get-slice [m dimension i]))

(defprotocol PSliceView
  "Protocol for quick view access into a row-major slices of an array. If implemented, must return
   either a view or an immutable sub-matrix: it must *not* return copied data.

   If the matrix is mutable, it must return a mutable view.

   The default implementation creates a wrapper view."
  (get-major-slice-view [m i] "Gets a view of a major array slice"))

(defprotocol PSliceSeq
  "Returns the row-major slices of the array as a sequence. 

   These must be views or immutable sub-arrays for higher order slices, or scalars
   for the slices of a 1D vector.

   The default implementation uses get-major-slice-view to obtain the slices."
  (get-major-slice-seq [m] "Gets a sequence of all major array slices"))

(defprotocol PSliceSeq2
  "Returns slices of the array as a sequence. 

   These must be views or immutable sub-arrays for higher order slices, or scalars
   for the slices of a 1D vector."
  (get-slice-seq [m dim] "Gets a sequence of all array slices"))

(defprotocol PSliceViewSeq
  "Returns the row-major slice views of the array. 

   These must be arrays if the array is mutable, i.e. slices of a 1D vector
   must be 0-dimensional mutable arrays."
  (get-major-slice-view-seq [m] "Gets a sequence of all major array slices"))

(defprotocol PSliceJoin
  "Protocol for concatenating / joining arrays."
  (join [m a] "Concatenates a to m, along the major slice dimension"))

(defprotocol PSubVector
  "Protocol for getting a sub-vector view of a vector. Must return a mutable view
   if the original vector is mutable. Should throw an exception if the specified
   subvector is out of bounds for the target vector."
  (subvector [m start length]))

;; TODO: should return either an immutable sub-matrix or a mutable view
(defprotocol PMatrixSubComponents
  "Protocol for picking out subsections of a 2D matrix. Should return a mutable view if possible.
   The default implementation creates a new vector containing the diagonal values."
  (main-diagonal [m]))

(defprotocol PSparseArray
  "Protocol for determining if an array is in a sparse format. It is up to the implementation to define
   its own sparse formats, but in general the intention should be that a sparse array uses significantly
   less storage than an equivalent dense array, assuming a high proportion of zero values in the array."
  (is-sparse? [m]))

(defprotocol PZeroCount
  "Protocol for counting the number of zeros in an array"
  (zero-count [m]))

(defprotocol PAssignment
  "Protocol for assigning values element-wise to mutable arrays."
  (assign!
    [m source]
    "Sets all the values in an array from a given source. Source may be a scalar
     or a smaller array that can be broadcast to the shape of m.")
  (assign-array!
    [m arr]
    [m arr start length]
    "Sets the elements in an array from an Java array source, in row-major order."))

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
  "Protocol for getting data as a double array"
  (to-double-array [m]
    "Returns a double array containing the values of m in row-major order. May or may not be
     the internal double array used by m, depending on the implementation.")
  (as-double-array [m]
    "Returns the internal double array used by m. If no such array is used, returns nil.
     Provides an opportunity to avoid copying the internal array."))

(defprotocol PObjectArrayOutput
  "Protocol for getting data as an object array"
  (to-object-array [m]
    "Returns an object array containing the values of m in row-major order. May or may not be
     the internal object array used by m, depending on the implementation.")
  (as-object-array [m]
    "Returns the internal object array used by m. If no such array is used, returns nil.
     Provides an opportunity to avoid copying the internal array."))

(defprotocol PValueEquality
  "Protocol for comparing two arrays, with the semantics of clojure.core/=.
   Returns false if the arrays are not of equal shape, or if any elements are not equal."
  (value-equals [m a]))

(defprotocol PMatrixEquality
  "Protocol for numerical array equality operations."
  (matrix-equals [a b]
     "Return true if a equals b, i.e. if a and b are have the same shape and all elements are equal.
      Must use numerical value comparison on numbers (==) to account for matrices that may hold a mix of
      numercial types (e.g. java.lang.Long and java.lang.Double). Implementations that only support doubles
      should use Number.doubleValue() to get a numeric value to compare.
      May throw an exception if the matrices are non-numeric"))

(defprotocol PMatrixEqualityEpsilon
  "Protocol for numerical array equality operations with a specified tolerance."
  (matrix-equals-epsilon [a b eps]
     "As matrix-equals, but provides a numerical tolerance for equality testing."))

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on an arbitrary matrix, vector or scalar.

   Implementation may return nil if the implementation does not support one of the parameters, in
   which case a more general operation will be attempted."
  (matrix-multiply [m a])
  (element-multiply [m a]))

(defprotocol PMatrixProducts
  "Protocol for general inner and outer products of arrays.
   Products should use + and * as normally defined for numerical types"
  (inner-product [m a])
  (outer-product [m a]))

(defprotocol PAddProduct
  "Protocol for add-product operation.
   Intended to implement a fast version for result = m + a * b"
  (add-product [m a b]))

(defprotocol PAddProductMutable
  "Protocol for mutable add-product! operation."
  (add-product! [m a b]))

(defprotocol PAddScaledProduct
  "Protocol for add-product operation.
   Intended to implement a fast version for result = m + a * b * factor"
  (add-scaled-product [m a b factor]))

(defprotocol PAddScaledProductMutable
  "Protocol for mutable add-product! operation."
  (add-scaled-product! [m a b factor]))

(defprotocol PAddScaled
  "Protocol for add-scaled operation.
   Intended to implement a fast version for result = m + a * factor"
  (add-scaled [m a factor]))

(defprotocol PAddScaledMutable
  "Protocol for mutable add-scaled! operation."
  (add-scaled! [m a factor]))

(defprotocol PMatrixDivide
  "Protocol to support element-wise division operator.
   One-arg version returns the reciprocal of all elements."
  (element-divide
    [m]
    [m a]))

(defprotocol PMatrixDivideMutable
  "Protocol to support mutable element-wise division operater.
   One-arg version returns the reciprocal of all elements."
  (element-divide!
    [m]
    [m a]))

(defprotocol PMatrixMultiplyMutable
  "Protocol to support mutable matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply! [m a])
  (element-multiply! [m a]))

(defprotocol PVectorTransform
  "Protocol to support transformation of a vector to another vector.
   Is equivalent to matrix multiplication when 2D matrices are used as transformations.
   But other transformations are possible, e.g. affine transformations.

   A transformation need not be a core.matrix matrix: other types are permissible"
  (vector-transform [t v] "Transforms a vector")
  (vector-transform! [t v] "Transforms a vector in place - mutates the vector argument"))

(defprotocol PMatrixScaling
  "Protocol to support numerical array scaling by scalar values. Provided because array classes may have
   efficient specialised scaling operaions.

   Works according the the default definition of multiplication for the matrix class
   (usually numerical, i.e. equivalent to clojure.core/+)"
  (scale [m a]
    "Multiplies a array by the scalar value a, ")
  (pre-scale [m a]
    "Pre-multiplies the array with the scalar. This is the same as scale for arrays
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
    "Returns the transpose of a matrix. Equivalent to reversing the \"shape\".
     Note that:
     - The transpose of a scalar is the same scalar
     - The transpose of a 1D vector is the same 1D vector
     - The transpose of a 2D matrix swaps rows and columns"))

(defprotocol PRotate
  "Rotates an array along a specified dimension by the given number of places.

   Rotating a dimension that does not exist has no effect on the array."
  (rotate [m dim places])) 

(defprotocol PRotateAll
  "Rotates an array using the specified shifts for each dimension.

   shifts may be any sequence of iteger shift amounts."
  (rotate-all [m shifts])) 

(defprotocol PTransposeInPlace 
  "Protocol for mutable 2D matrix transpose in place"
  (transpose! [m]
    "Transposes a mutable 2D matrix in place"))

(defprotocol POrder
  "Protocol for matrix reorder"
  (order
    [m cols]
    [m dimension cols]))

(defprotocol PNumerical
  "Protocol for identifying numerical arrays. Should return true if every element in the
   array is a valid numerical value."
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
     Must throw an error if the matrix is not square (i.e. all dimensions sizes are equal)")
  (determinant [m]
    "Returns the determinant of a matrix.")
  (inverse [m]
    "Returns the invese of a matrix. Should throw an exception if m is not invertible."))

(defprotocol PNegation
  (negate [m]
    "Returns a new numerical array with all elements negated."))

(defprotocol PMatrixRank
  "Protocol to support computing the rank (number of linearly independent rows) in a matrix"
  (rank [m]
        "Returns the rank of a matrix"))

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

;; code generation for protocol with unary mathematics operations defined in c.m.i.mathsops namespace
;; also generate in-place versions e.g. signum!
(eval
  `(defprotocol PMathsFunctions
  "Protocol to support mathematic functions applied element-wise to a numerical array"
  ~@(map (fn [[name func]] `(~name [~'m])) mops/maths-ops)))

(eval
  `(defprotocol PMathsFunctionsMutable
  "Protocol to support mutable mathematic functions applied element-wise to a numerical array"
  ~@(map (fn [[name func]] `(~(symbol (str name "!")) [~'m])) mops/maths-ops)))

(defprotocol PElementCount
  "Protocol to return the total count of elements in matrix. Result may be any integer type,
   typically a java.lang.Long"
  (element-count [m]))

(defprotocol PElementMinMax
  "Protocol to return the minimum and maximum elements in a numerical array. Must throw an exception 
   if the array is not numerical."
  (element-min [m])
  (element-max [m]))

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
    "Reduces with the function f over all elements of m."))


(defprotocol PMatrixPredicates
  "Protocol for matrix predicates like identity-matrix? or zero-matrix?"
  (identity-matrix?
    [m]
    "returns true if the matrix m is an identity-matrix")
  (zero-matrix?
    [m]
    "returns true if all the elements of matrix m are zeros")
  (symmetric?
    [m]
    "returns true if matrix m is symmetric"))

(defprotocol PMatrixTypes
  (diagonal? [m] "Returns true if the matrix is diagonal")
  (upper-triangular? [m] "Returns true if the matrix m is upper triangualar")
  (lower-triangular? [m] "Returns true if the matrix m is lower triangualar")
  (positive-definite? [m] "Returns true is the matrix is positive definite")
  (positive-semidefinite? [m] "Returns true is the matrix is positive semidefinite"))

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

;; ============================================================
;; Utility functions

(defn persistent-vector-coerce [x]
  "Coerces a data structure to nested persistent vectors"
  (let [dims (dimensionality x)]
    (cond
      (== dims 0) (get-0d x)
      (clojure.core/vector? x) (mapv convert-to-nested-vectors x)
      (instance? java.util.List x) (mapv convert-to-nested-vectors x)
      (instance? java.lang.Iterable x) (mapv convert-to-nested-vectors x)
      (instance? clojure.lang.Seqable x) (mapv convert-to-nested-vectors x)
      (.isArray (class x)) (mapv convert-to-nested-vectors (seq x))
      :default (error "Can't coerce to vector: " (class x)))))

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

(defn same-shapes?
  "Returns true if a sequence of arrays all have the same shape."
  [arrays]
  (let [shapes (map #(or (get-shape %) []) arrays)]
    (loop [s (first shapes) ns (next shapes)]
      (if ns
        (if (same-shape-object? s (first ns))
          (recur s (next ns))
          false)
        true)))) 
