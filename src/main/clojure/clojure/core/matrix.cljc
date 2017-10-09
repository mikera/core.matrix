(ns clojure.core.matrix
  "Main namespace for the core.matrix array programming API. 

   Contains all of the most common array programing functions. Arguments can generally be:
    - A scalar value, e.g. the double 1.7
    - A valid core.matrix array, e.g. [[1 2] [3 4]] as a 2D matrix or [1 2 3] as a 1D vector

   Functions in this API may be supported by multiple matrix implementations, allowing code that uses
   this API to quickly switch between implementations without significant changes (if any). The precise
   imnplementation used is generally the first array argument to any API function - this is intended to
   be consistent with conventions for Clojure protocol dispatch. The precise results of operations
   may be implementation-dependent, subject to the constraints stated in the docstrings."
  (:require [clojure.core.matrix.impl.defaults :as default]
            [clojure.core.matrix.impl.persistent-vector]
            [clojure.core.matrix.impl.sequence] ;; TODO: figure out if we want this?
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp :refer [*matrix-implementation*]]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.utils :as u])
  (:refer-clojure :exclude [array clone array?])
  #?(:clj (:require
            [clojure.core.matrix.macros :refer [TODO error]]
            [clojure.core.matrix.impl.index]
            [clojure.core.matrix.impl.pprint :as pprint]
            [clojure.core.matrix.impl.double-array]
            [clojure.core.matrix.impl.object-array])
     :cljs (:require-macros
             [clojure.core.matrix :refer [def-mat-mop def-mat-mops]]
             [clojure.core.matrix.macros :refer [TODO error]])))

;; ==================================================================================
;; clojure.core.matrix API namespace
;;
;; This is the public API for clojure.core.matrix
;;
;; General handling of operations is as follows:
;;
;; 1. User calls public API function defined in clojure.core.matrix
;; 2. The clojure.core.matrix function delegates to a protocol for the appropriate function
;;    with protocols as defined in the clojure.core.matrix.protocols namespace. In most cases
;;    clojure.core.matrix will try to delegate as quickly as possible to the implementation.
;; 3. The underlying matrix implementation implements the protocol to handle the API
;;    function call.
;; 4. The implementation decides what to do for the concrete operation.
;; 5. If the implementation does not understand one or more parameters, it may either throw an
;;    exception or (in some cases) return nil to delegate to a fallback implementation
;;
;; ==================================================================================
#? (:clj (do
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed))
:cljs (def class type))

;; (set! *unchecked-math* :warn-on-boxed) ;; use to check for boxing if needed

;; =============================================================
;; matrix construction functions

(declare current-implementation)
(declare implementation-check)
(declare current-implementation-object)
(declare to-nested-vectors)
(declare slice slice-view)

(defn array
  "Constructs a new n-dimensional array from the given data.

   This function will examine the data in order to construct an array of the appropriate shape.

   The data may be in one of the following forms:
   - A valid existing array (which will be converted to the implementation)
   - Nested sequences of scalar values, e.g. Clojure vectors (must have regular shape)
   - A sequence of slices, each of which must be valid array data
   - A single scalar value, which will be wrapped or coerced as necessary for the implementation

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*

   If the implementation does not support the shape or type of data provided, may either
   create an array using a different implementation on a best-efforts basis or
   alternatively throw an error. This behaviour is implementation-specific."
  ([data]
    (or
      (mp/construct-matrix (implementation-check) data)
      (mp/coerce-param [] data)))
  ([implementation data]
    (or
      (mp/construct-matrix (implementation-check implementation) data)
      (mp/coerce-param [] data))))

(defn matrix
  "Constructs a new 2-dimensional matrix from the given numerical data.

   The data may be in one of the following forms:
   - A valid existing numerical array
   - Nested sequences of scalar values, e.g. Clojure vectors
   - A sequence of slices, each of which must be valid matrix data

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*

   `matrix` works as a synonym for `array`"
  ([data]
    (array data))
  ([implementation data]
    (array implementation data)))

(defn index
  "Constructs a new 1-dimensional integer index from given data.

   The data may be in one of the following forms:
   - A valid existing index
   - A 1D array of integer values
   - A sequence of integer values

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation* to produce the index object.

   If the implementation does not support its own native index types, will return a
   valid index from a default implementation."
  ([data]
    (or
      (mp/index-coerce (implementation-check) data)
      (mp/index-coerce [] data)))
  ([implementation data]
    (or
      (mp/index-coerce (implementation-check implementation) data)
      (mp/index-coerce [] data))))

(defn zero-vector
  "Constructs a new zero-filled numerical vector with the given length.

   Implementations are encouraged to return immutable vectors or sparse vectors
   for efficency whre available."
  ;; TODO: implement a specialised constructor protocol for zero vectors / arrays
  ([length]
    (mp/new-vector (implementation-check) length))
  ([implementation length]
    (mp/new-vector (implementation-check implementation) length)))

(defn new-vector
  "Constructs a new vector with the given length.
   New matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable vectors, then the new vector will be fully mutable."
  ([length]
    (mp/new-vector (implementation-check) length))
  ([implementation length]
    (mp/new-vector (implementation-check implementation) length)))

(defn zero-matrix
  "Returns a zero-filled numerical matrix with the given dimensions, for the given implementation or the current
   implementation if not specified.

   May produce a lightweight immutable zero matrix if supported by the implementation."
  ([rows columns]
    (mp/new-matrix (implementation-check) rows columns))
  ([implementation rows columns]
    (mp/new-matrix (implementation-check implementation) rows columns)))

(defn new-matrix
  "Constructs a new 2D array (matrix) with the given dimensions.
   The new matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable matrices, then the new matrix will be fully mutable."
  ([rows columns]
    (mp/new-matrix (implementation-check) rows columns))
  ([implementation rows columns]
    (mp/new-matrix (implementation-check implementation) rows columns)))

(defn zero-array
  "Creates a new zero-filled numerical array with the given shape."
  ([shape]
    (or (mp/new-matrix-nd (implementation-check) shape)
        (mp/new-matrix-nd [] shape)))
  ([implementation shape]
    (or (mp/new-matrix-nd (implementation-check implementation) shape)
        (mp/new-matrix-nd [] shape))))

(defn new-array
  "Creates a new array with the given shape.
   New array will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable matrices, then the new matrix will be fully mutable."
  ([shape]
    (or (mp/new-matrix-nd (implementation-check) shape)
        (mp/new-matrix-nd [] shape)))
  ([implementation shape]
    (or (mp/new-matrix-nd (implementation-check implementation) shape)
        (mp/new-matrix-nd (implementation-check) shape)
        (mp/new-matrix-nd [] shape)))) ;; todo: what is the right default?

(defn new-sparse-array
  "Creates a new sparse array with the given shape.
   New array will contain default values as defined by the implementation (usually zero).
   If the implementation supports mutable sparse matrices, then the new matrix should be fully mutable."
  ([shape]
    (mp/new-sparse-array (implementation-check) shape))
  ([implementation shape]
    (or (mp/new-sparse-array (implementation-check implementation) shape)
        (error "Implementation " (mp/implementation-key implementation)
               " does not support sparse arrays of shape " (vec shape)))))

(defn new-scalar-array
  "Returns a new mutable scalar array containing the scalar value zero."
  ([]
    (new-scalar-array imp/*matrix-implementation*))
  ([implementation]
    (let [implementation (implementation-check implementation)]
      (mp/new-scalar-array implementation))))

(defn scalar-array
  "Creates a new zero-dimensional array containing the specified scalar value."
  ([value]
    (mp/new-scalar-array (implementation-check) value))
  ([implementation value]
    (mp/new-scalar-array (implementation-check implementation) value)))

(defn row-matrix
  "Constructs a row matrix with the given data. The returned matrix is a 2D 1xN row matrix.

   The data must be either a valid existing vector or a sequence of scalar values."
  ([data]
    (mp/row-matrix (implementation-check) data)) ;; wrapping in 1 extra vector level, should be OK
  ([implementation data]
    (mp/row-matrix (implementation-check implementation) data)))

(defn column-matrix
  "Constructs a column matrix with the given data. The returned matrix is a 2D Nx1 column matrix.

   The data must be either a valid existing vector or a sequence of scalar values."
  ([data]
    (mp/column-matrix (implementation-check) data)) ;; TODO: is use of map broken here? Might not be sequential?
  ([implementation data]
    (mp/column-matrix (implementation-check implementation) data)))

(defn identity-matrix
  "Constructs a 2D identity matrix with the given number of rows.

   Identity matrices constructed with this function may not be fully mutable because they may be
   implemented with a specialised identity matrix type. Use (mutable (identity-matrix ...)) if you
   need to guarantee a mutable matrix."
  ([dims]
    (mp/identity-matrix (implementation-check) dims))
  ([implementation dims]
    (mp/identity-matrix (implementation-check implementation) dims)))

(defn permutation-matrix
  "Constructs a permutation matrix for a given permutation vector. The permutation vector should
   contain a distinct set of integers 0...n-1, representing the re-ordering performed by
   the permutation matrix."
  ([permutation]
    (mp/permutation-matrix (implementation-check) permutation))
  ([implementation permutation]
    (mp/permutation-matrix (implementation-check implementation) permutation)))

(defn block-diagonal-matrix
  "Constructs a block diagonal matrix for a given vector of 2D square matrices and arranges
  the matrices along the main diagonal of the 2D matrix"
  ([blocks]
    (mp/block-diagonal-matrix (implementation-check) blocks))
  ([implementation blocks]
    (mp/block-diagonal-matrix (implementation-check implementation) blocks)))

(defn mutable
  "Constructs a fully mutable copy of the given array data. 

   If the current implementation does not support mutable matrices, will return a mutable array
   from another core.matrix implementation that supports either the same element type or a broader type."
  ([data]
    (try (or (mp/mutable-matrix data)
            (mutable (implementation-check) data))
      (catch Throwable t ;; catch error in array construction, attempt to use a default implementation
        (default/construct-mutable-matrix data))))
  ([implementation data]
    (let [imp (implementation-check implementation)
          r (mp/construct-matrix imp data)]
      (or (and r (mp/ensure-mutable r)) ;; ensure contructed array is mutable, else fall through with nil
          (default/construct-mutable-matrix data)
          (error "Unable to create mutable array for implementation " (mp/implementation-key imp))))))

(defn immutable
  "Returns an immutable array containing the given array data.

   May return the same array if it is already immutable.

   If the implementation does not support immutable matrices, will return an immutable array
   from another core.matrix implementation that supports either the same element type or a broader type."
  ([data]
    (or (mp/immutable-matrix data)
        (to-nested-vectors data))))

(defn ensure-mutable
  "Checks if an array is mutable, and if not converts to a new mutable array. Guarantees
   that the result will be mutable, but may not be the same type as the original array."
  ([m]
    (mp/ensure-mutable m)))

(defn diagonal-matrix
  "Constructs a 2D diagonal matrix with the given numerical values on the main diagonal.
   All off-diagonal elements will be zero, and diagonal-values may be a vector or any
  Clojure sequence of values.

   Diagonal matrices constructed this way may use specialised storage formats, hence may not be fully mutable.
   Use (mutable (diagonal-matrix ...)) if you need to guarantee a mutable matrix."
  ([diagonal-values]
    (mp/diagonal-matrix (implementation-check) diagonal-values))
  ([implementation diagonal-values]
    (mp/diagonal-matrix (imp/get-canonical-object implementation) diagonal-values)))

(defn compute-matrix
  "Creates a array with the specified shape, and each element specified by (f i j k...)
   Where i, j, k... are the index positions of each element in the matrix"
  ([shape f]
    (compute-matrix (implementation-check) shape f))
  ([implementation shape f]
    (let [m (implementation-check implementation)]
      (mp/compute-matrix m shape f))))

(defn sparse-array
  "Creates a sparse array with the given data, using a specified implementation
  or the current implementation if not specified.

  Throws an exception if creation of a sparse array is not possible"
  ([data]
    (sparse-array (implementation-check) data))
  ([implementation data]
    (or (mp/sparse-coerce (implementation-check implementation) data)
        (error "Sparse implementation not available"))))

(defn sparse-matrix
  "Creates a sparse matrix with the given data, using a specified implementation
  or the current implementation if not specified. Sparse matrices are required to store
  a M*N matrix with E non-zero elements in approx O(M+N+E) space or less.

  Throws an exception if creation of a sparse matrix is not possible.

  `sparse-matrix` wqorks as a synonym for `sparse-array`."
  ([data]
    (sparse-array data))
  ([implementation data]
    (sparse-array implementation data)))

(defn sparse
  "Coerces an array to a sparse format if possible. Sparse arrays are expected to
   minimise space usage for zero elements.

   Returns the array unchanged if such coercion is not possible, or if the array is already sparse."
  ([data]
    (mp/sparse data))
  ([implementation data]
    (let [implementation (implementation-check implementation)]
      (or (mp/sparse-coerce implementation data) (mp/coerce-param implementation data)))))

(defn dense
  "Coerces an array to a dense format if possible. Dense arrays are expected to
   allocate contiguous storage space for all elements. Either row-major or column-major
   storage may be alloacted, depending on the implementation.

   'dense' should not be used with very large arrays, and may throw an OutOfMemoryError
    if the dense array is too large to fit in available memory.

   Returns the array unchanged if such coercion is not possible, or if the array is already dense."
  ([data]
    (mp/dense data))
  ([implementation data]
    (or (mp/dense-coerce implementation data) (mp/coerce-param implementation data))))

(defn native
  "Coerces an array into a native format array if possible. Native arrays may offer
   superior performance for some operations, depending on the implementation.
   Returns nil if no appropriate native format exists."
  ([a]
    (or (mp/native a) (native (implementation-check) a)))
  ([impl a]
    (let [a (mp/coerce-param impl a)]
      (mp/native a))))

(defn native?
  "Returns true if the array is in a native format.

   Native formats are implementation defined, and may use non-Java resources (e.g. GPU memory)."
  ([a]
    (mp/native? a)))

#?(:clj
(defmacro with-implementation
  "Runs a set of expressions using a specified matrix implementation.

   Example:
     (with-implementation :vectorz
       (new-matrix 10 10))"
  [impl & body]
  `(binding [imp/*matrix-implementation* (imp/get-canonical-object ~impl)]
     ~@body))
)

;; ======================================
;; Implementation details

(defn supports-dimensionality?
  "Returns true if the implementation for a given matrix supports a specific dimensionality, i.e.
   can natively create and manipulate matrices with the given number of dimensions"
  ([m dimension-count]
    (let [m (implementation-check m)]
      (mp/supports-dimensionality? m dimension-count))))

(defn supports-shape?
  "Returns true if the implementation supports creation of matrices with a specific shape."
  [m shape]
  (let [m (if (keyword? m) (imp/get-canonical-object m) m)]
    (mp/supports-dimensionality? m (count shape))))

;; ======================================
;; matrix assignment and copying

;; note we would have called this `set!` except for the name clash with clojure.core
(defn assign!
  "Assigns a new value to an array. Sets the values of the target element-wise, broadcasting where necessary.
   Returns the mutated array. The new value may be either a scalar or a array of compatible (maybe smaller) shape."
  ([dest src]
    (mp/assign! dest src)
    dest))

(defn assign-array!
  "Assigns values to a destination core.matrix array from a Java array, in element-wise order. 
   The element type of Java array must be compatible.
   Returns the mutated core.matrix array"
  ([dest java-array]
    (mp/assign-array! dest java-array)
    dest)
  ([dest java-array offset]
    (mp/assign-array! dest java-array offset (mp/element-count dest))
    dest))

(defn assign
  "Copies array src element-wise, broadcasting to fill the whole shape of m.
   Similar to assign!, except returns a new destination array.
   Returns a new array, of the same shape and implementation as the original m."
  ([dest src]
    (mp/assign dest src)))

(defn clone
  "Constructs a (shallow) clone of the array. This function is intended to
   allow safe defensive usage of matrices / vectors. If the intent is to create a mutable clone of
   some array data, it is recommended to use mutable instead.

   Guarantees that:
   1. Mutating the returned array will not modify any other array (defensive copy)
   2. The returned array will be fully mutable, if the implementation supports mutable matrices.

   The clone may or may not be of the same implementation: implementations are encouraged to do so but
   this is not mandatory.

   A core.matrix implementation which only provides immutable arrays may safely return the same array."
  ([m]
    (mp/clone m)))

(defn to-nested-vectors
  "Converts an array to an idiomatic, immutable nested Clojure vector format. The bottom level of the
   nested vectors will contain the element values. Higher levels will all implement IPersistentVector.

   The depth of nesting will be equal to the dimensionality of the array."
  ([m]
    (mp/convert-to-nested-vectors m)))

(defn scalar
  "Coerces m to a scalar value. Result is guaranteed not to be an array.
   Will throw an exception if m is not zero-dimensional."
  {:inline (fn [m] `(mp/get-0d ~m))}
  ([m]
    (mp/get-0d m)))

;; ==============================
;; Matrix predicates and querying

(defn array?
  "Returns true if the parameter is a valid core.matrix N-dimensional array, for any N>=0."
  {:inline (fn [m] `(not (mp/is-scalar? ~m)))}
  ([m]
    (not (mp/is-scalar? m))))

(defn matrix?
  "Returns true if parameter is a valid core.matrix matrix (i.e. an array with dimensionality == 2)"
  ([m]
    (== (long (mp/dimensionality m)) 2)))

(defn vec?
  "Returns true if the parameter is a core.matrix vector (1-dimensional array)"
  ([m]
    (mp/is-vector? m)))

(defn scalar?
  "Returns true if the parameter is a scalar value (i.e. acceptable as matrix element value).
   A 0-d array containing a scalar is *not* itself a scalar value."
  {:inline (fn [m] `(mp/is-scalar? ~m))}
  ([v]
    (mp/is-scalar? v)))

(defn zero-dimensional?
  "Returns true if the parameter has zero dimensions. i.e. it is a 0-d array or a scalar value.

   Behaviour is the same as `scalar?`, except that true is returned for 0-dimensional arrays."
  [m]
  (== 0 (long (mp/dimensionality m))))

(defn identity-matrix?
  "Returns true if the parameter is an identity-matrix, i.e. a symmetric square matrix with element values
   of 1 on the leading diagonal and 0 elsewhere."
  ([m]
    (mp/identity-matrix? m)))

(defn zero-matrix?
  "Returns true if all the elements of the parameter are zero."
  ([m]
    (mp/zero-matrix? m)))

(defn symmetric?
  "Returns true if the parameter is a symmetric matrix, i.e. Aij = Aji for all i,j."
  ([m]
    (mp/symmetric? m)))

(defn diagonal?
  "Returns true if the parameter is a diagonal matrix."
  ([m]
    (mp/diagonal? m)))

(defn upper-triangular?
  "Returns true if the parameter is a upper triangular matrix."
  ([m]
    (mp/upper-triangular? m)))

(defn lower-triangular?
  "Returns true if the parameter is a lower triangular matrix."
  ([m]
     (mp/lower-triangular? m)))

(defn orthogonal?
  "Returns true if the parameter is an orthogonal matrix."
  ([m eps]
     (mp/orthogonal? m eps))
  ([m]
     (mp/orthogonal? m 1.E-5)))

(defn sparse?
  "Returns true if an array is sparse, i.e. the implementation supports storage of the entire
   array in less memory than would normally be implied by the number of elements.

   Sparse matrices may have memory requirements that scale with the number of non-zero elements
   rather than the total number of elements, for example."
  ([m]
    (mp/is-sparse? m)))

(defn element-type
  "Returns the class of elements that can be in the array. For example, a numerical array may return
   the class java.lang.Double."
  (^java.lang.Class [m]
    (mp/element-type m)))

(defn dimensionality
  "Returns the dimensionality of an array. The dimensionality is equal to
   the number of dimensions in the array's shape."
  {:inline (fn ([m] `(long (mp/dimensionality ~m))))}
  (^long [m]
    (long (mp/dimensionality m))))

(defn dimension-count
  "Returns the size of the specified dimension in a matrix. Will throw an error if the matrix
   does not have the specified dimension."
  {:inline (fn ([m dim] `(long (mp/dimension-count ~m ~dim))))}
  (^long [m dim]
    (long (mp/dimension-count m dim))))

(defn row-count
  "Returns the number of rows in a matrix or vector (array must be 1D or more)."
  {:inline (fn ([m] `(dimension-count ~m 0)))}
  (^long [m]
    (dimension-count m 0)))

(defn column-count
  "Returns the number of columns in a matrix (array must be 2D or more)"
  {:inline (fn ([m] `(dimension-count ~m 1)))}
  (^long [m]
    (dimension-count m 1)))

(defn slice-count
  "Returns the number of slices in an array (array must be 1D or more). The array is sliced
   in row-major order, i.e. this is the dimension count of the first dimension."
  {:inline (fn ([m] `(dimension-count ~m 0)))}
  (^long [m]
    (dimension-count m 0)))

(defn ecount
  "Returns the total count of elements in an array, as an integer value.

   Equal to the product of the lengths of each dimension in the array's shape.

   Result will usually be a Long, however callers should note that for very large sparse arrays
   the element count may be a BigInteger, i.e. equal to or larger than 2^63.

   Returns 1 for a zero-dimensional array or scalar. "
  ([m]
    (mp/element-count m)))

(defn square?
  "Returns true if matrix is square (i.e. a 2D array with same number of rows and columns)"
  ([m]
    (and
      (== 2 (dimensionality m))
      (== (dimension-count m 0) (dimension-count m 1)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. is 2D and has exactly one row)"
  ([m]
    (and (== (long (mp/dimensionality m)) 2)
         (== 1 (dimension-count m 0)))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. is 2D and has has exactly one column)"
  ([m]
    (and (== (long (mp/dimensionality m)) 2)
         (== 1 (dimension-count m 1)))))

(defn rank
  "Returns the rank of each element in an array, as an array of indexes with 0 being first, in ascending order.

   If passed a higher dimensional array, returns an array of ranks along the last dimension.

   An optional comparator may be provided, otherwise the default comparator will be used. Comparator should
   support java.util.Comparator on the JVM, although some implementations may support alternative comparator types 
   e.g. Clojure predicates. Please check the documentation for your core.matrix implementation."
  ([a]
    (mp/index-rank a))
  ([comparator a]
    (mp/index-rank a comparator)))

(defn shape
  "Returns the shape of an array, i.e. the dimension sizes for all dimensions.

   The result will be a vector containing only integer index values, with a count
   equal to the dimensionality of the array.

   Returns nil the if object is not an array (i.e. is a scalar value)"
  {:inline (fn
             ([m] `(if-let [~'sh (mp/get-shape ~m)]
                     (vec ~'sh)
                     nil)))}
  ([m]
    (if-let [sh (mp/get-shape m)]
      (vec sh)
      nil)))

(defn validate-shape
  "Like shape, but validates the shape of the array and throws an exception if the array does not have the
   expected shape. Useful for ensuring that mixed implementation arrays or nested Clojure vectors conform to
   the expected shape.

   Returns the shape of the array if sucessful, or nil if the object is not a core.matrix array."
  ([m]
    (mp/validate-shape m))
  ([m shape]
    (mp/validate-shape m shape)))

(defn zero-count
  "Returns the number of zeros in an array.

   Result will usually be a Long, however callers should note that for very large sparse arrays
   the zero count may be a BigInteger, i.e. equal to or larger than 2^63."
  ([m]
    (mp/zero-count m)))

(defn density
  "Returns the density of the matrix, defined as the proportion of non-zero elements"
  (^double [m]
    (let [zeros (double (mp/zero-count m))
          elems (double (mp/element-count m))]
      (double (/ (- elems zeros) elems)))))

(defn mutable?
  "Returns true if the matrix is mutable, i.e. supports setting of values.

   It is possible for some matrix implementations to have constraints on mutability (e.g. mutable only in diagonal elements),
   this method will still return true for such cases."
  ([m]
    (mp/is-mutable? m)))

(defn index?
  "Returns true if the parameter is a valid array index type. An index is a seq-able 1D list
   of integer values that can be used to index into arrays."
  ([m]
    (mp/index? m)))

(defn conforming?
  "Returns true if two arrays have a conforming shape. Two arrays are conforming if there
   exists a common shape that both can broadcast to. This is a requirement for element-wise
   operations to work correctly on two different-shaped arrays."
  ([a] true)
  ([a b] (let [sa (mp/get-shape a) sb (mp/get-shape b)]
           (and (>= (count sa) (count sb))
                (every? identity (map = (reverse sa) (reverse sb)))))))

(defn same-shape?
  "Returns true if the arrays have the same shape, false otherwise"
  ([] true)
  ([m] true)
  ([m n]
    (or
      (identical? m n)
      (mp/same-shape? m n)))
  ([m n & more]
    (loop [m m n n more (seq more)]
      (if (or (identical? m n) (mp/same-shape? m n))
        (if more
          (recur n (first more) (next more))
          true)
        false))))

(defn numerical?
  "Returns true if the matrix is a valid numerical matrix (i.e. supports numerical core.matrix operations)."
  ([m]
    (mp/numerical? m)))

;; =======================================
;; Conversions

(defn to-double-array
   "Returns a Java double[] array containing the values of a numerical array m in row-major order. Will
    throw an error if any of the array elements cannot be converted to doubles.

    If want-copy? is true, will guarantee a new double array (defensive copy).
    If want-copy? is false, will return the internal array used by m, or nil if not supported
    by the implementation.
    If want-copy? is not specified, will return either a copy or the internal array"
   (^doubles [m]
     (mp/to-double-array m))
   (^doubles [m want-copy?]
     (let [arr (mp/as-double-array m)]
       (if want-copy?
         (if arr (u/copy-double-array arr) (mp/to-double-array m))
         arr))))

(defn to-object-array
   "Returns a Java Object[] array containing the values of an array m in row-major order.

    If want-copy? is true, will guarantee a new Object array (defensive copy).
    If want-copy? is false, will return the internal array used by m, or nil if not supported
    by the implementation.
    If want-copy? is not specified, will return either a copy or the internal array"
   (^objects [m]
     (mp/to-object-array m))
   (^objects [m want-copy?]
     (let [arr (mp/as-object-array m)]
       (if want-copy?
         (if arr (u/copy-object-array arr) (mp/to-object-array m))
         arr))))

(defn pack
  "Packs array data in the most efficient format as defined by the implementation. May return the
   same array if no additional packing is required."
  ([a]
    (mp/pack a)))

(defn coerce
  "Coerces param (which may be any array) into a format preferred by a specific matrix implementation.
   If the matrix implementation is not specified, uses the current matrix implementation.
   If param is already in a format deemed usable by the implementation, may return it unchanged.

   coerce should never alter the shape of the array, but may convert element types where necessary
   (e.g. turning real values into complex values when converting to a complex array type)."
  ([param]
    (let [m (imp/get-canonical-object)]
      (or
       (mp/coerce-param m param)
       (mp/coerce-param m (mp/convert-to-nested-vectors param)))))
  ([matrix-or-implementation param]
    (let [m (if (keyword? matrix-or-implementation) (imp/get-canonical-object matrix-or-implementation) matrix-or-implementation)]
      (or
        (mp/coerce-param m param)
        (mp/coerce-param m (mp/convert-to-nested-vectors param))))))

;; =======================================
;; matrix access

(defn mget
  "Gets a scalar value from an array at the specified position. Supports any number of dimensions."
  {:inline (fn
             ([m] `(mp/get-0d ~m))
             ([m x] `(mp/get-1d ~m ~x))
             ([m x y] `(mp/get-2d ~m ~x ~y)))
   :inline-arities #{1 2 3}}
  ([m]
    (mp/get-0d m))
  ([m x]
    (mp/get-1d m x))
  ([m x y]
    (mp/get-2d m x y))
  ([m x y & more]
    (mp/get-nd m (cons x (cons y more)))))

(defn mset
  "Sets a scalar value in an array at the specified position. Supports any number of dimensions.

   Returns a new matrix and leaves the original unchanged.

   WARNING: performance of this operation may be as high as O(N) where N is the number of elements in
   the array. Consider using mutable arrays and `mset!` when setting large numbers of individual elements
   is required."
  ([m v]
    (mp/set-0d m v))
  ([m x v]
    (mp/set-1d m x v))
  ([m x y v]
    (mp/set-2d m x y v))
  ([m x y z & more]
    (mp/set-nd m (cons x (cons y (cons z (butlast more)))) (last more))))

(defn mset!
  "Mutates a scalar value in an array at the specified position. Supports any number of dimensions.

   Will throw an exception if the matrix is not mutable at the specified position. Note that it
   is possible for some arrays to be mutable in places and immutable in others (e.g. sparse arrays)

   Returns the modified matrix (it is guaranteed to return the same instance)"
  {:inline (fn
             ([m v] `(mp/set-0d! ~m ~v))
             ([m x v] `(mp/set-1d! ~m ~x ~v))
             ([m x y v] `(mp/set-2d! ~m ~x ~y ~v)))
   :inline-arities #{2 3 4}}
  ([m v]
    (mp/set-0d! m v)
    m)
  ([m x v]
    (mp/set-1d! m x v)
    m)
  ([m x y v]
    (mp/set-2d! m x y v)
    m)
  ([m x y z v]
    (mp/set-nd! m [x y z] v)
    m)
  ([m x y z t & more]
    (mp/set-nd! m (into [x y z t] (butlast more)) (last more))
    m))

(defn get-row
  "Gets a row of a matrix, as a 1D vector.

   The format of the returned 1D vector is determined by the implementation of the source matrix. 
   get-row may return a lightweight mutable view if supported by the implementation."
  ([m x]
    (mp/get-row m x)))

;; TODO: consider if should support labelled columns?
(defn get-column
  "Gets a column of a matrix, as a 1D vector.

   The format of the returned 1D vector is determined by the implementation of the source matrix. 
   get-column may return a lightweight mutable view if supported by the implementation."
  ([m col-index]
    (mp/get-column m col-index)))

(defn- slice-dims
  "Slices along all dimensions where there is a numerical argument"
  [m args slice-func]
  (let [shape (mp/get-shape m)
        N (count args)]
    (when (not= N (dimensionality m))
      (error "Inconsistent count of selection arguments " args " for shape " shape))
    (loop [m m
           i 0
           args (seq args)]
      (if args
        (let [ix (first args)]
          (if (number? ix) ;;slice current dimension?
            (recur (slice-func m i ix) i (next args))
            (recur m (inc i) (next args))))
        m))))

(defn- normalise-arg
  "Normalises arg to either a number of a sequable list of indexes"
  [arg ^long dim-count]
  (cond
    (number? arg) arg
    (= :all arg) (vec (range dim-count))
    (= :last arg) (dec dim-count)
    (= :butlast arg) (vec (range (dec dim-count)))
    (= :first arg) 0
    (= :rest arg) (vec (range 1 dim-count))
    :else arg))

(defn- normalise-args
  "Normalises arguments by mapping :all to the complete range"
  [args m]
  (mapv normalise-arg args (mp/get-shape m)))

(defn select
  "Returns an array containing all elements in a which are at the positions
   of the Cartesian product of args. An argument can be:
    - a number - slices at this dimension (eliminates the dimension),
    - a keyword which selects specific slices (:first :last)
    - a 1-dimensional array of numbers which selects the slices at these indices
    - a keyword which selects a range of slices (:all :butlast :rest)

   The number of args must match the dimensionality of a.

   Examples:
   (select [[1 2][3 4]] 0 0) ;=> 1
   (select [[1 2][3 4]] 0 :all) ;=> [1 2]
   (select [[1 2][3 4]] [0 1] [0]) ;=> [[1] [3]]
   (select [[1 2][3 4]] :all 0) ;=> [1 3]"
  [a & args]
  (let [args (normalise-args args a)
        a (slice-dims a args slice)
        selecting-args (filterv (complement number?) args)]
    (mp/select a selecting-args)))

(defn select-view
  "Like `select`, but guarantees a view over the original data."
  [a & args]
  (let [args (normalise-args args a)
        a (slice-dims a args slice-view)
        selecting-args (filterv (complement number?) args)]
    (or
      (mp/select-view a selecting-args)
      (wrap/wrap-selection a selecting-args))))

(defn select-indices
  "Returns a one-dimensional array of the elements which are at the specified
   indices. An index is a one-dimensional array which element-count matches the
   dimensionality of a. Examples:
   (select-indices [[1 2] [3 4]] [[0 0][1 1]]) ;=> [1 4]"
  [a indices]
  (mp/get-indices a indices))

(defn set-selection!
  "Like set-selection but mutates the array in place. Will throw an error if array is immutable."
  [a & args]
  (let [value (last args)
        args (butlast args)]
    (assign! (apply select-view a args) value)
    a))

(defn set-selection
  "Like select but sets the elements in the selection to the value of the final argument.
   Leaves a unchanged and returns the modified array"
  [a & args]
  (let [a (mutable a)
        r (apply set-selection! a args)]
    (coerce a r)))

(defn set-indices
  "like select-indices but sets the elements at the specified indices to values.
   Leaves the original array (a) unchanged and returns a modified array"
  [a indices values]
  (mp/set-indices a indices values))

(defn set-indices!
  "like set-indices but destructively modifies array in place"
  [a indices values]
  (mp/set-indices! a indices values)
  a)

;; =====================================
;; matrix slicing and views

(defn submatrix
  "Gets a view of a submatrix, for a set of index ranges.
   
   If Index ranges are used they must be a sequence [start, length] pairs,
   with the special exception that these pairs can be nil (gets the whole range).

   May be a mutable view if supported by the implementation."
  ([m index-ranges]
    (mp/submatrix m index-ranges))
  ([m dimension index-range]
    (mp/submatrix m (assoc (vec (repeat (mp/dimensionality m) nil)) dimension index-range)))
  ([m row-start row-length col-start col-length]
    (mp/submatrix m [(list row-start row-length) (list col-start col-length)])))

(defn subvector
  "Gets a view of part of a vector, specifed by a start index and length.

   The view maintains a reference to the original,
   so can be used to modify the original vector if it is mutable."
  ([m start length]
    (mp/subvector m start length)))

(defn slice
  "Gets a slice of an array along a specific dimension.
   
   The returned array will have one less dimension, i.e. slicing a matrix will return a vector
   and slicing a 1D vector will return a scalar.

   Slicing on the first dimension (dimension 0) is likely to perform better
   for many array implementations. This is the default."
  ([m index]
    (mp/get-slice m 0 index))
  ([m dimension index]
    (mp/get-slice m dimension index)))

(defn slices
  "Gets a sequence of slices of an array. If dimension is supplied, slices along a given dimension,
   otherwise slices along the first dimension.

   Returns a sequence of scalar values if the array is 1-dimensional."
  ([m]
    (mp/get-major-slice-seq m))
  ([m dimension]
    (mp/get-slice-seq m dimension)))

(defn slice-views
  "Gets a sequence of views of the slices of an array. If dimension is supplied, slices along a given dimension,
   otherwise slices along the first dimension. If the matrix implementation supports mutable views, these views
   can be used to mutate portions of the original array.

   A key difference between 'slices' and 'slice-views' is that 'slice-views' must always return views. In order
   to ensure this behaviour on mutable 1-dimensional arrays, it must return a sequence of 0-dimensional arrays."
  ([m]
    (mp/get-major-slice-view-seq m))
  ([m dimension]
    (if (== 0 (long dimension))
      (slice-views m)
      (map #(mp/get-slice-view m dimension %) (range (mp/dimension-count m dimension))))))

(defn slice-view
  "Gets a view of an array slice. Guaranteed to return a mutable view if the array is mutable."
  ([m i]
    (mp/get-major-slice-view m i))
  ([m dimension i]
    (if (== 0 (long dimension))
      (mp/get-major-slice-view m i)
      (mp/get-slice-view m dimension i))))

(defn rows
  "Gets the rows of a matrix, as a sequence of 1D vectors.

   If the array has more than 2 dimensions, will return the rows from all slices in order."
  ([m]
    (mp/get-rows m)))

(defn columns
  "Gets the columns of a matrix, as a seqable collection of 1D vectors.

   If the array has more than 2 dimensions, will return the columns from all slices in order."
  ([m]
   (mp/get-columns m)))

(defn main-diagonal
  "Returns the main diagonal of a matrix or general array, as a vector.
   The main diagonal of a general array is defined as those elements where the all the
   indexes are equal, i.e. the index is of the form [i i ... i]
   Works on both square and rectangular matrices."
  ([m]
    (mp/main-diagonal m)))

(defn diagonal
  "Returns the specified diagonal of a 2D matrix as a vector.
   If k>0, returns a diagonal above the main diagonal.
   If k<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices.
   Returns empty vector if value of k is out of range (outside matrix)"
  ([m]
    (mp/main-diagonal m))
  ([m k]
    (let [k (long k)]
      (cond
        (neg? k) (mp/main-diagonal (mp/submatrix m [[(- k) (+ (dimension-count m 0) k)]
                                                    [0 (dimension-count m 1)]]))
        (pos? k) (mp/main-diagonal (mp/submatrix m [[0 (dimension-count m 0)]
                                                    [k (- (dimension-count m 1) k)]]))
        :else   (mp/main-diagonal m)))))

(defn join
  "Joins arrays together, along the major dimension 0. For 1D vectors, this behaves as simple concatenation.
   Other dimensions must be compatible. To join arrays along a different dimension, use 'join-along' instead."
  ([& arrays]
    (reduce mp/join arrays)))

(defn join-along
  "Joins arrays together, concatenating them along the specified dimension.

   Other dimensions must be compatible."
  ([dimension & arrays]
    (or
      (reduce #(mp/join-along %1 %2 dimension) arrays)
      (error "Failure to joins arrays"))))

(defn conjoin
  "Adds a new value [b] as a new slice to an array [a], returning the extended array.
   Broadcasts the new value to the correct shape of a slice of a if necessary.

   This can be considered as the array equivalent of clojure.core/conj"
  ;; TODO: implement using a protocol
  ([a b]
    (let [ss (assoc (vec (mp/get-shape a)) 0 1)]
      (join a (mp/broadcast b ss))))
  ([a b & more]
    (let [ss (vec (next (mp/get-shape a)))
          slcs (mapv #(mp/broadcast % ss) (cons b more))]
      (join a slcs))))

(defn conjoin-along
  "Adds a new value [b] as a new slice to an array [a] along the given dimension,
   returning the extended array.
   Broadcasts the new value to the correct shape of a slice of a if necessary.

   This can be considered as the array equivalent of clojure.core/conj using
   a specified dimension"
  ;; TODO: implement using a protocol
  ([dim a b]
    (if (== (long dim) 0)
      (conjoin a b)
      (let [ss (mp/get-shape (mp/get-slice a dim 0))]
        (join-along dim a (mp/broadcast b ss)))))
  ([dim a b & more]
    (reduce
      (fn [a b] (conjoin-along dim a b))
      (conjoin-along dim a b)
      more)))

(defn rotate
  "Rotates an array along specified dimensions.

   Elements rotated off will re-appear at the other side. The shape of the array will not be modified."
  ([m dimension shift-amount]
    (mp/rotate m dimension shift-amount))
  ([m shifts]
    (mp/rotate-all m shifts)))

(defn shift
  "Shifts all elements of an array along specified dimensions, maintaining the shape of the array.

   New spaces shifted into the array are filled with zero."
  ([m dimension shift-amount]
    (mp/shift m dimension shift-amount))
  ([m shifts]
    (mp/shift-all m shifts)))

(defn order
  "Reorders slices of an array along a specified dimension. Re-orders along major dimension
   if no dimension is specified.

   Indicies can be any seqable object containing the indices along the specified dimension to select.
   An index can be selected multiple times (which created repreated slices), or not at all (which excludes
   the slice from the result)."
  ([m indices]
    (mp/order m indices))
  ([m dimension indices]
    (mp/order m dimension indices)))

(defn as-vector
  "Creates a view of an array as a single flattened vector.

   Returns nil if this is not supported by the implementation. You should use `to-vector` instead to
   obtain a flattened vector without guaranteeing a view."
  ([m]
    (mp/as-vector m)))

(defn to-vector
  "Creates a new array representing the elements of array m as a single flattened vector.

   This operation guarantees a new copy of the data."
  ([m]
    (or
      (mp/to-vector m)
      (array m (mp/to-object-array m)))))

;; ====================================
;; structural change operations

(defn broadcast
  "Broadcasts an array to a specified shape. Returns a new array with the shape specified.
   The broadcasted array may be a view over the original array: attempting to modify the
   broadcasted array therefore has implementation-dependent results.

   Will throw an exception if broadcast to the target shape is not possible."
  ([m shape]
    (or (mp/broadcast m shape)
        (mp/broadcast (mp/coerce-param (implementation-check) m) shape))))

(defn broadcast-like
  "Broadcasts the second array to the shape of the first. See 'broadcast'."
  {:inline (fn ([m a] `(mp/broadcast-like ~m ~a)))}
  ([m a]
    (mp/broadcast-like m a)))

(defn broadcast-coerce
  "Broadcasts and coerces the second array to the same shape and type of the first.
   Equivalent to (coerce m (broadcast-like m a)).

   Useful for converting arrays to the correct shape and type for efficient future operations."
  {:inline (fn ([m a] `(mp/broadcast-coerce ~m ~a)))}
  ([m a]
    (mp/broadcast-coerce m a)))

(defn transpose
  "Transposes an array, returning a new array. 

   Assuming no specific ordering is provided:
    - 1D vectors and scalars will be returned unchanged
    - For 2D matrices, rows and columns are swapped.
    - The dimension indices are reversed for any shape of a higher order N-dimensional array. 

   If ordering is provided, will re-order dimensions according to the provided order. The 
   provided ordering must be of the same length as the dimensionality of the array and
   contain all the integers in the range 0...(dims-1)."
  ([m]
    (mp/transpose m))
  ([m ordering]
    (mp/transpose-dims m ordering)))

(defn transpose!
  "Transposes a square 2D matrix in-place.

   Will throw an exception if not possible (e.g. if the matrix is not square or not mutable)."
  ([m]
    (mp/transpose! m)
    m))

(defn reshape
  "Changes the shape of a matrix to the specified new shape. shape can be any sequence of dimension sizes.

   Preserves the row-major order of matrix elements. Truncates the sequence of elements if the shape is smaller
   than the original shape.

   Pads with default values (dependent on implementation - normally zero) if the shape is larger."
  ([m shape]
    (mp/reshape m shape)))

(defn reshape-view
  "Returns a view over an array with the specified new shape. shape can be any sequence of dimension sizes.

   Preserves the row-major order of matrix elements. Truncates the sequence of elements if the shape is smaller
   than the original shape.

   Behaviour is currently undefined if the new shape is larger than the original array."
  ([m shape]
    (mp/reshape-view m shape)))

(defn fill!
  "Fills an array with a single scalar value. The scalar value must be compatible with the element-type
   of the array.

   Similar to assign!, but only supports scalar values (and may be more efficient)."
  ([m value]
    (mp/fill! m value)
    m))

(defn fill
  "Fills an array with a single scalar value. The scalar value must be compatible with the element-type
   of the array. Returns a new array.

   Functionally similar to `assign!` except only intended for use with a scalar value."
  ([m value]
    (assign m (mp/get-0d value))))

;; ======================================
;; matrix comparisons

;; Note on naming convention, we don't call it `equals?` because it isn't a 1-arg predicate...
(defn equals
  "Returns true if two arrays are numerically equal.

   Will return false for arrays of different shapes.

   May either return false or throw an error if the arrays are not numerical.

   If epsilon is provided, performs an equality test
   with the given maximum tolerance (default is 0.0, i.e. exact numerical equivalence)"
  ([a] true)
  ([a b]
    (mp/matrix-equals a b))
  ([a b epsilon]
    (mp/matrix-equals-epsilon a b epsilon)))

(defn cmp
  "Element-wise of comparisons of two arrays. Returns the signum of the difference
   between corresponding elements in two arrays.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (cmp 1 3) ;=> -1
  (cmp 0 0) ;=> 0
  (cmp 1 -1) ;=> 1
  (cmp [[1 3] [5 5]] [[3 3] [5 3]]) ;=> [[-1 0] [0 1]]
  (cmp [[1 4][1 5][1 8]] [[1 2][1 5][2 7]]) ;=> [[0 1][0 0][-1 1]]
  "
  [a b]
  (mp/element-compare a b))

(defn eif
  "Element-wise if. Tranverses each element, x, of an array, m. If x > 0,
  returns a (if a is a scalar) or the corresponding element from a (if a is an
  array or matrix). If x <= 0, returns b (if b is a scalar) or the corresponding
  element from array b (if b is an array or matrix).

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (eif (lt 1 3) 3 6) ;=> 3
  (eif (lt 5 3) 3 6) ;=> 6
  (eif (eq A B) 1 2) ;=> [[1 2] [2 1]]
  (eif (eq A B) 1 D) ;=> [[1 1] [9 1]]
  (eif (eq A B) C 2) ;=> [[2 2] [2 2]]
  (eif [[1 0][0 1] [[2 3][4 5]] [[6 7][8 9]]) ;=> [[2 7][8 5]]
  (eif (gt [[2 6][3 5]] 4) [[0 0][0 0]] [[1 1][1 1]] ;=> [[0 1][0 1]]"
  ([m a b]
    (mp/element-if m a b)))

(defn lt
  "Element-wise less-than comparison operation. Returns a binary array where
  elements less-than the argument are represented by 1 and elements greater-
  than or equal to the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (lt 1 4) ;=> 1
  (lt 3 3) ;=> 0
  (lt [[1 5] [3 6]] 3) ;=> [[1 0] [0 0]]
  (lt [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[1 0] [1 0]]"
  ([m a]
    (mp/element-lt m a))
  ([m a & more]
    (let [arrays (cons m (cons a more))]
      ;; use multiplication to get locical "and"
      (reduce mp/element-multiply (map (partial apply mp/element-lt) (partition 2 1 arrays))))))

(defn le
  "Element-wise less-than-or-equal-to comparison operation. Returns a binary
  array where elements less-than or equal to the argument are represented by 1
  and elements greater-than to the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (le 3 3) ;=> 1
  (le 4 3) ;=> 0
  (le [[1 5] [3 6]] 3) ;=> [[1 0] [1 0]]
  (le [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[1 0] [1 1]]"
  ([m a]
    (mp/element-le m a))
  ([m a & more]
    (let [arrays (cons m (cons a more))]
      ;; use multiplication to get locical "and"
      (reduce mp/element-multiply (map (partial apply mp/element-le) (partition 2 1 arrays))))))

(defn gt
  "Element-wise greater-than comparison operation. Returns a binary array where
  elements greater-than the argument are represented by 1 and elements less-
  than or equal to the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (gt 4 3) ;=> 1
  (gt 3 3) ;=> 0
  (gt [[1 5] [3 6]] 3) ;=> [[0 1] [0 1]]
  (gt [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[0 1] [0 0]]"
  ([m a]
    (mp/element-gt m a))
  ([m a & more]
    (let [arrays (cons m (cons a more))]
      ;; use multiplication to get locical "and"
      (reduce mp/element-multiply (map (partial apply mp/element-gt) (partition 2 1 arrays))))))

(defn ge
  "Element-wise greater-than-or-equal-to comparison operation. Returns a binary
  array where elements greater-than or equal to the argument are represented by 1
  and elements less-than to the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (ge 2 3) ;=> 0
  (ge 3 3) ;=> 1
  (ge [[1 5] [3 6]] 3) ;=> [[0 1] [1 1]]
  (ge [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[0 1] [0 1]]"
  ([m a]
    (mp/element-ge m a))
  ([m a & more]
    (let [arrays (cons m (cons a more))]
      ;; use multiplication to get locical "and"
      (reduce mp/element-multiply (map (partial apply mp/element-ge) (partition 2 1 arrays))))))

(defn ne
  "Element-wise not-equal comparison operation. Returns a binary array where
  elements not-equal to the argument are represented by 1 and elements equal to
  the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (ne 1 1) ;=> 0
  (ne 5 1) ;=> 1
  (ne [[1 5] [3 6]] 3) ;=> [[1 1] [0 1]]
  (ne [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[1 1] [1 0]]"
  ([m a]
    (mp/element-ne m a)))

(defn eq "Element-wise equal comparison operation. Returns a binary
array where elements equal to the argument are represented by 1 and
elements not-equal to the argument are 0.

  Performs broadcasting of arguments if required to match the size of the largest array.

  Examples:
  (eq 1 1) ;=> 1
  (eq 5 1) ;=> 0
  (eq [[1 5] [3 6]] 3) ;=> [[0 0] [1 0]]
  (eq [[1 5] [4 6]] [[2 3] [5 6]]) ;=> [[0 0] [0 1]]"
  ([m a]
    (mp/element-eq m a)))

(defn e=
  "Returns true if all corresponding array elements are equal (using the semantics of clojure.core/=).

   WARNING: a java.lang.Long does not equal a java.lang.Double.
   Use 'equals' or 'e==' instead if you want to test for numerical equality."
  ([m1]
    true)
  ([m1 m2]
    (mp/value-equals m1 m2))
  ([m1 m2 & more]
    (and
      (mp/value-equals m1 m2)
      (apply e= m2 more))))

(defn e==
  "Returns true if all corresponding array elements are numerically equal.

   Throws an error if any elements of the arrays being compared are not numerical values."
  ([m1]
    true)
  ([m1 m2]
    (equals m1 m2))
  ([m1 m2 & more]
    (reduce equals (equals m1 m2) more)))

;; ======================================
;; Matrix labels
;;
;; Label support is optional - unlabelled arrays must return Long values 0,1,2... for labels along each dimension
(defn label
  "Returns a label for the specified position along a given array dimension. Returns nil if the dimension is unlabelled."
  ([m dim i]
    (mp/label m dim i)))

(defn labels
  "Return a vector of labels for a given array dimension. Return nil if the dimension is unlabelled."
  ([m dim]
    (mp/labels m dim)))

(defn label-index
  "Return the index of a label along a given dimension. Returns nil if the label does not exist."
  ([m dim label]
    (let [ls (mp/labels m dim)]
      (and ls (u/find-index ls label)))))


;; ======================================
;; matrix maths / operations

(defn mul
  "Performs element-wise multiplication with scalars and numerical arrays.

   Examples: 
     (mul [1 2] [3 4]) ;=> [3 8]

   Behaves like clojure.core/* for scalar values."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce mp/element-multiply (mp/element-multiply a b) more)))

(defn ^{:deprecated true} emul
  "DEPRECATED: please use mul instead."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce mp/element-multiply (mp/element-multiply a b) more)))

(defn mmul
  "Performs matrix multiplication on matrices or vectors. 

   Equivalent to inner-product, but may be more efficient for matrices."
  ([] 1.0)
  ([a] a)
  ([a b]
    (or (mp/matrix-multiply a b) 
        ;; fallback to inner product if matrix-multiply does not produce a result
        (mp/inner-product a b)))
  ([a b & more]
    (reduce mp/matrix-multiply (mp/matrix-multiply a b) more)))

(defn e*
  "An element-wise multiply operator equivalent to `mul`."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce e* (e* a b) more)))

(defn div
  "Performs element-wise division on numerical arrays.

   Computes the reciprocal of each element when passed a single argument (similar to clojure.core//)."
  ([a] (mp/element-divide a))
  ([a b] (mp/element-divide a b))
  ([a b & more] (reduce mp/element-divide (mp/element-divide a b) more)))

(defn div!
  "Performs in-place element-wise matrix division for numerical arrays.

   All arguments after the first must be broadcastable to the shape of the first array.

   Computes the reciprocal of each element when passed a single argument (similar to clojure.core//)."
  ([a]
     (mp/element-divide! a)
     a)
  ([a b]
     (mp/element-divide! a b)
     a)
  ([a b & more]
     (mp/element-divide! a b)
     (doseq [c more]
       (mp/element-divide! a c))
     a))

(defn mul!
  "Performs in-place element-wise multiplication of numerical arrays.

   All arguments after the first must be broadcastable to the shape of the first array.

   Returns the first argument after mutation."
  ([a] a)
  ([a b]
    (mp/element-multiply! a b)
    a)
  ([a b & more]
    (mp/element-multiply! a b)
    (doseq [c more]
      (mp/element-multiply! a c))
    a))

(defn ^{:deprecated true} emul!
  "Performs in-place element-wise multiplication of numerical arrays.

   Returns the first argument after mutation."
  ([a] a)
  ([a b]
    (mp/element-multiply! a b)
    a)
  ([a b & more]
    (mp/element-multiply! a b)
    (doseq [c more]
      (mp/element-multiply! a c))
    a))

(defn transform
  "Transforms a given vector with a transformation, returning a new vector.

   The transformation may be a 2D matrix, but other types of transformation may also be supported
   e.g. affine transformations, unary operators."
  ([t v]
    (mp/vector-transform t v)))

(defn transform!
  "Transforms a given vector in place. This is a mutable equivalent to `transform`.

   Returns the transformed vector.

   The transformation must map an n-dimensional vector to another n-dimensional vector, i.e.
   if it is a 2D matrix then it must have shape [n x n]."
  ([t v]
    (mp/vector-transform! t v)
    v))

(defn add
  "Performs element-wise addition on one or more numerical arrays.

   Will broadcast to the smallest shape compatible will addition of all input arrays."
  ([] 0.0)
  ([a] a)
  ([a b]
    (mp/matrix-add a b))
  ([a b & more]
    (reduce mp/matrix-add (mp/matrix-add a b) more)))

(defn add!
  "Performs element-wise mutable addition on one or more numerical arrays. This is the mutable
   equivalent of `add`.

   All arguments after the first must be broadcastable to the shape of the first array.

   When adding many arrays, use of `add!` with a mutable array as the first argument is
   usually faster than repreated use of `add` because it can avoid unnecessary copying.

   Returns the first array after it has been mutated."
  ([a] a)
  ([a b]
    (mp/matrix-add! a b)
    a)
  ([a b & more]
    (mp/matrix-add! a b)
    (doseq [m more] (mp/matrix-add! a m))
    a))

(defn add-product
  "Adds the element-wise product of two numerical arrays to the first array.
   
   Arrays should be the same shape, some implementations may support broadcasting."
  ([m a b]
    (mp/add-product m a b)))

(defn add-product!
  "Adds the product of two numerical arrays to the first array. Returns the mutated array.

   Arrays should be the same shape, some implementations may support broadcasting."
  ([m a b]
    (mp/add-product! m a b)
    m))

(defn add-scaled
  "Adds a numerical array scaled by a given factor to the first array. 

   Factor should be a scalar numerical value."
  ([m a factor]
    (mp/add-scaled m a factor)))

(defn scale-add
  "Scales array m1 by factor a, then adds an array m2 scaled by factor b. May optionally add a constant.
   Broadly equivalent to (add (mul m1 a) (mul m2 b) constant)

   Returns a new array."
  ([m1 a m2 b]
    (mp/scale-add m1 a m2 b 0.0))
  ([m1 a m2 b constant]
    (mp/scale-add m1 a m2 b constant)))

(defn scale-add!
  "Scales array m1 by factor a, then adds an array m2 scaled by factor b. May optionally add a constant.
   Broadly equivalent to (add! (mul! m1 a) (mul m2 b) constant)

   Returns the mutated array `m1`. The array `m2` will not be changed."
  ([m1 a m2 b]
    (scale-add! m1 a m2 b 0.0))
  ([m1 a m2 b constant]
    (mp/scale-add! m1 a m2 b constant)
    m1))

(defn lerp
  "Performs linear interpolation between two arrays. If factor is 0.0, result will be equal to the first vector.
   If factor is 1.0, result will be equal to the second vector. Returns a new array."
  ([a b factor]
    (mp/lerp a b factor)))

(defn lerp!
  "Performs linear interpolation between two arrays. If factor is 0.0, result will be equal to the first vector.
   If factor is 1.0, result will be equal to the second vector. Returns a the mutated first array."
  ([a b factor]
    (mp/lerp! a b factor)
    a))

(defn add-scaled!
  "Adds a numerical array scaled by a given factor to the first array. Returns the mutated array."
  ([m a factor]
    (mp/add-scaled! m a factor)
    m))

(defn add-scaled-product
  "Adds the product of two numerical arrays scaled by a given factor to the first array.

   This is equivalent to (add m (mul a b factor)) but may be optimised by the underlying implementation."
  ([m a b factor]
    (mp/add-scaled-product m a b factor)))

(defn add-scaled-product!
  "Adds the product of two numerical arrays scaled by a given factor to the first array.
   Returns the mutated array.
   This is equivalent to (add! m (mul a b factor)) but may be optimised by the underlying implementation."
  ([m a b factor]
    (mp/add-scaled-product! m a b factor)
    m))

(defn add-inner-product!
  "Adds the inner product of two numerical arrays to the first array.
   Returns the mutated array.
   This is equivalent to (add! m (inner-product a b)) but may be optimised by the underlying implementation."
  ([m a b]
    (mp/add-inner-product! m a b)
    m)
  ([m a b factor]
    (mp/add-inner-product! m a b factor)
    m))

(defn add-outer-product!
  "Adds the inner product of two numerical arrays to the first array.
   
   Returns the mutated array.

   This is equivalent to (add! m (outer-product a b)) but may be optimised by the underlying implementation."
  ([m a b]
    (mp/add-outer-product! m a b)
    m)
  ([m a b factor]
    (mp/add-outer-product! m a b factor)
    m))

(defn set-inner-product!
  "Sets an array equal to the inner product of two numerical arrays.
   Returns the mutated first array.
   This is equivalent to (assign! m (inner-product a b)) but may be optimised by the underlying implementation."
  ([m a b]
    (mp/set-inner-product! m a b)
    m)
  ([m a b factor]
    (mp/set-inner-product! m a b factor)
    m))

(defn sub
  "Performs element-wise subtraction on one or more numerical arrays.

   For a single argument, returns the negation.

   Returns a new array."
  ([a] (mp/negate a))
  ([a b]
    (mp/matrix-sub a b))
  ([a b & more]
    (reduce mp/matrix-sub (mp/matrix-sub a b) more)))

(defn sub!
  "Performs element-wise mutable subtraction on one or more numerical arrays.

   NOTE: For a single argument, returns the argument unchanged: use negate! instead if you wish to negate a mutable
   array in place. This is intentional, so that you can do (apply sub! m list-of-arrays) and get the expected
   result if the list of arrays is empty.

   Returns the first array, after it has been mutated."
  ([a] a)
  ([a b]
    (mp/matrix-sub! a b)
    a)
  ([a b & more]
    (mp/matrix-sub! a b)
    (doseq [m more] (mp/matrix-sub! a m))
    a))

(defn scale
  "Scales a array by one or more scalar factors. The default implementation supports numerical arrays and
   numbers as scalar values, however matrix implementations may extend this to support other scalar types.

   Returns a new scaled matrix."
  ([m factor]
    (mp/scale m factor))
  ([m factor & more-factors]
    (mp/scale m (mp/element-multiply factor (reduce mp/element-multiply more-factors)))))

(defn scale!
  "Scales a numerical array by one or more scalar factors (in place). The default implementation supports
   numerical arrays and numbers as scalar values, however matrix implementations may extend this to
   support other scalar types (e.g. complex numbers).

   Returns the array after it has been mutated."
  ([m factor]
    (mp/scale! m factor)
    m)
  ([m factor & more-factors]
    (mp/scale! m (mp/element-multiply factor (reduce mp/element-multiply more-factors)))
    m))

(defn square
  "Squares every element of a numerical array. Returns a new array."
  ([m]
    (mp/square m)))

(defn normalise
  "Normalises a numerical vector (scales to unit length, i.e. the L2 norm). 

   Returns a new normalised vector.

   The result is undefined if the initial length of the vector is zero (it is possible the
   implementation may return NaNs or zeros). If this is a concern, it is recommended to check
   the length of the vector first in order to handle this as a special case."
  ([v]
    (mp/normalise v)))

(defn normalise!
  "Like 'normalise', but mutates a numerical vector in-place (scales to unit length).
   Returns the modified vector."
  ([v]
    (mp/normalise! v)
    v))

(defn dot
  "Efficiently computes the scalar dot product (1Dx1D inner product) of two numerical vectors. Prefer this API
   function if you are performing a dot product on 1D vectors and require a scalar result.

   If either argument is not a vector, will compute and return a higher dimensional inner-product."
  ([a b]
    (or
      (mp/vector-dot a b) ;; this allows an optimised implementation of 'dot' for vectors, which should be faster
      (let [v (mp/inner-product a b)]
        (cond
          (number? v) v ;; fast path if v is a numerical result
          (== 0 (long (mp/dimensionality v))) (mp/get-0d v) ;; ensure 0 dimensional results are scalar
          :else v ;; higher dimensional result, OK to return directly as an array
          )))))

(defn inner-product
  "Computes the inner product of numerical arrays.

   For matrix/matrix and matrix/vector arguments, this is equivalent to matrix multiplication.

   The inner product of two arrays with indexed dimensions {..i j} and {j k..} has dimensions {..i k..}. The inner-product of two vectors will be scalar."
  ([] 1.0)
  ([a]
    a)
  ([a b]
    (mp/inner-product a b))
  ([a b & more]
    (reduce mp/inner-product (mp/inner-product a b) more)))

(defn outer-product
  "Computes the outer product of numerical arrays.

   For two vectors of size m and n, the result will be a m x n matrix.

   In general, the outer product fior higher diemnsional arrays is defined as the 
   tensor product (see https://en.wikipedia.org/wiki/Tensor_product)
   i.e. The outer product of two arrays with indexed dimensions {i..j} and {k..l} 
   has dimensions {i..j k..l}. The dimensionality of the result will be the
   sum of the dimensionalities of the two arguments."
  ([] 1.0)
  ([a] a)
  ([a b]
    (or (mp/outer-product a b)
        ;; if outer-product is not supported, fall back to using Clojure persistent vectors
        (mp/outer-product (mp/convert-to-nested-vectors a) b)))
  ([a b & more]
    (reduce outer-product (outer-product a b) more)))

(defn cross
  "Computes the 3D cross-product of two numerical vectors.

   Behavior on other types is undefined."
  ([a b]
    (mp/cross-product a b)))

(defn cross!
  "Computes the cross-product of two numerical 3D vectors a and b, storing the result in the first vector.

   Returns the (mutated) first vector."
  ([a b]
    (mp/cross-product! a b)
    a)
  ([dest a b]
    (mp/assign! dest a)
    (mp/cross-product! dest b)))

(defn distance
  "Calculates the euclidean distance between two numerical vectors, as a single numerical scalar value.

   This is equivalent to (norm 2 (sub a b)) but may be optimised by the underlying implementation."
  ([a b]
    (mp/distance a b)))

(defn det
  "Calculates the determinant of a 2D square numerical matrix, as a single numerical scalar value."
  ([a]
    (or ;; try the current implementation, if not fall back to best available numeric implementation
      (mp/determinant a)
      (mp/determinant (mp/coerce-param imp/*numeric-implementation* a)))))

(defn inverse
  "Calculates the inverse of a square 2D numerical matrix.

   Returns nil if the matrix is singular. 

   Throws an error is the argument is not a sqaure 2D matrix."
  ([m]
    (mp/inverse m)))

(defn negate
  "Calculates the negation of a numerical array. Generally equivalent to (scale m -1.0)"
  ([m]
    (mp/negate m)))

(defn negate!
  "Calculates the negation of a numerical array in place. Generally equivalent to (scale! m -1.0)"
  ([m]
    (mp/scale! m -1.0)
    m))

(defn trace
  "Calculates the trace of a 2D numerical matrix (sum of elements on main diagonal).

   The matrix need not be square."
  ([a]
    (mp/trace a)))

(defn magnitude
  "Calculates the magnitude over all elements in an array.

   This is the 2-norm: equivalent to the Frobenius norm on matrices, or the Euclidean length on vectors."
  ([m]
    (mp/length m)))

(defn magnitude-squared
  "Calculates the squared magnitude over all elements in an array.

   This may be more efficient that computing the magnitude in some implementations."
  ([m]
     (mp/length-squared m)))

(defn ^{:deprecated true} length
  "DEPRECATED: please use magnitude instead."
  ([m]
    (mp/length m)))

(defn ^{:deprecated true} length-squared
  "DEPRECATED: please use magnitude-squared instead."
  ([m]
     (mp/length-squared m)))

(defn pow
  "Raises every element of a numerical matrix by the given exponent.

   Note that behaviour for large exponents may depend on the underlying implementation:
   for example double-based matrices may overflow to Double/POSITIVE_INFINITY."
  ([m]
    m)
  ([m exponent]
    (mp/element-pow m exponent))
  ([m exponent & more]
    (reduce (fn [m x] (mp/element-pow m x)) (mp/element-pow m exponent) more)))

(defn pow!
  "Mutable exponent function, see 'pow'"
  ([m a]
    ;; TODO: implement via a protocol + default implementation
    (mp/assign! m (pow m a))
    m))

#?(:clj (do

(defmacro def-mat-mop
  "Define a mathematical operator using core.matrix.protocols so it works over arrays and values."
  [op-sym fn-sym]
  `(do
     (defn ~op-sym
       ~(str "Computes the " op-sym " function on all elements of an array, using double precision values. Returns a new array.")
       ([~'m]
        (~(symbol "clojure.core.matrix.protocols" (str op-sym)) ~'m)))

     (defn ~(symbol (str op-sym "!"))
       ~(str "Computes the " op-sym " function on all elements of an array, using double precision values. Mutates the array in-place.")
       ([~'m]
        (~(symbol "clojure.core.matrix.protocols" (str op-sym "!")) ~'m)
        ~'m))))

(defmacro def-mat-mops
  []
  `(do
     ~@(for [[name# func#] mops/maths-ops]
         `(def-mat-mop ~name# ~func#))))
))

;; create all unary maths operators
(def-mat-mops)

(defn logistic
  "Computes the sigmoid (logistic) function for every element of an array."
  [a]
  (mp/logistic a))

(defn logistic!
  "Computes the sigmoid (logistic) function for every element of an array. Mutates the array."
  [a]
  (mp/logistic! a)
  a)

(defn softplus
  "Computes the softplus function for every element of an array."
  [a]
  (mp/softplus a))

(defn softplus!
  "Computes the softplus function for every element of an array. Mutates the array."
  [a]
  (mp/softplus! a)
  a)

(defn relu
  "Computes the ReLU (rectified linear) function for every element of an array."
  [a]
  (mp/relu a))

(defn relu!
  "Computes the ReLU (rectified linear) function for every element of an array. Mutates the array."
  [a]
  (mp/relu! a)
  a)

(defn softmax
  "Computes the softmax function for a numerical vector."
  [a]
  (mp/softmax a))

(defn softmax!
  "Computes the softmax function for every element of a numerical vector. Mutates the vector."
  [a]
  (mp/softmax! a)
  a)

;; ==================================
;; Elementary row operations
;;

(defn swap-rows
  "Swap row i with row j in a matrix, returning a new matrix

   This is one of the three elementary row operation (see https://en.wikipedia.org/wiki/Elementary_matrix)."
  [m i j]
  (mp/swap-rows m i j))

(defn multiply-row
  "Multiply row i in a matrix by a constant factor, returning a new matrix

   This is one of the three elementary row operation (see https://en.wikipedia.org/wiki/Elementary_matrix)."
  [m i factor]
  (mp/multiply-row m i factor))

(defn add-row
  "Add a row j (optionally multiplied by a scalar factor) to a row i
   and replace row i with the result. Returns a new matrix.

   This is one of the three elementary row operation (see https://en.wikipedia.org/wiki/Elementary_matrix)."
  ([m i j]
    (mp/add-row m i j 1.0))
  ([m i j factor]
    (mp/add-row m i j factor)))

(defn set-row
  "Sets a row in a matrix using a specified vector."
  [m i row]
  (mp/set-row m i row))

(defn set-row!
  "Sets a row in a matrix in-place using a specified vector."
  [m i row]
  (mp/set-row! m i row)
  m)

(defn set-column
  "Sets a column in a matrix using a specified vector."
  [m i column]
  (mp/set-column m i column))

(defn set-column!
  "Sets a column in a matrix using a specified vector."
  [m i column]
  (mp/set-column! m i column)
  m)

;; ===================================
;; Sparse matrix functions

(defn non-zero-count
  "Counts the number of non-zero values in a numerical array.
   May perform a full array scan, but will often be quicker for specialised
   sparse arrays - sometimes as fast as O(1)"
  ([m]
    (mp/nonzero-count m)))

(defn non-zero-indices
  "Gets the non-zero indices of an array.
   - For a 1D vector, returns an ordered index list.
   - For a higher dimensional array, returns the non-zero-indices for each slice in row-major order."
  ([m] 
    (mp/non-zero-indices m)))

;; ====================================
;; Functional operations
;;
;; these work like regular clojure seq, map, reduce etc. but operate on all elements of
;; a matrix in row-major ordering

(defn eseq
  "Returns all elements of an array as a Clojure sequence in row-major order.
   
   Like clojure.core/seq, Returns nil if the array has no elements."
  ([m]
    ;; note we call seq to convert a seqable object returned by mp/element-seq into a seq
    (seq (mp/element-seq m))))

(defn ereduce
  "Element-wise reduce on all elements of an array. 

   It is *not* guaranteed that the reduction may be stopped early using clojure.core/reduced. If this 
   behaviour is wanted, please check the details of the specific implementation or use the more generic 
   Clojure code:
     (reduce f (eseq m))"
  ([f m]
    (mp/element-reduce m f))
  ([f init m]
    (mp/element-reduce m f init)))

(defn emap
  "Element-wise map over all elements of one or more arrays.

   f must return a result compatible with the element-type of the array m. If a more general 
   type is required, try coercing to a more general array type first, e.g.
     
     (emap (fn [x] (str x)) (double-array [1 2 3]))             ;; Throws an error
     (emap (fn [x] (str x)) (coerce [] (double-array [1 2 3]))) ;; OK!

   Implemenations may *optionally* support custom function types. If this is the case, the
   parameter m must be a valid array from the given implementation.

   Returns a new array of the same element-type and shape as the array m."
  ([f m]
    (mp/element-map m f))
  ([f m a]
    (mp/element-map m f a))
  ([f m a & more]
    (mp/element-map m f a more)))

(defn emap-indexed
  "Element-wise map-indexed over all elements of one or more arrays. Like
   emap, but provides an index as the second argument to the mapping function.

   f must accept as first argument the index vector of the current element,
   and return a result compatible with the element-type of the array m

   Returns a new array of the same element-type and shape as the array m."
  ([f m]
    (mp/element-map-indexed m f))
  ([f m a]
    (mp/element-map-indexed m f a))
  ([f m a & more]
    (mp/element-map-indexed m f a more)))

(defn slice-map
  "Maps a function over all slices of one or more array"
  ([f m]
    (mp/slice-map m f))
  ([f m1 m2]
    (mp/slice-map m1 f m2))
  ([f m1 m2 & more]
    (mp/slice-map m1 f m2 more)))

(defn filter-slices
  "Filters the slices of a core.matrix array, returning only those slices where (pred slice) is truthy.
   
   Will result in an nil value if no slices satify the criteria. If not nil, the return value is 
   guaranteed to be a seqable core.matrix array, e.g. you can:
    - use (seq (filter-slices m)) to get a sequence of slices
    - use (when-let [z (filter-slices m)] ...) to operate on the result as a core.matrix array."
  ([pred m]
    (mp/filter-slices m pred)))

(defn esum
  "Calculates the sum of all the elements in a numerical array."
  ([m]
    (mp/element-sum m)))

(defn emin
  "Gets the minimum element value from a numerical array"
  ([m]
    (mp/element-min m)))

(defn emax
  "Gets the maximum element value from a numerical array"
  ([m]
    (mp/element-max m)))

(defn minimum
  "Gets the minimum element value from a numerical array"
  ([m]
    (mp/element-min m)))

(defn maximum
  "Gets the maximum element value from a numerical array"
  ([m]
    (mp/element-max m)))

(defn clamp
  "Clamps each element in a numerical array between lower and upper bounds
  specified by a and b, respectively.

  Examples:
  (clamp [[1 5 1] [4 10 2] [5 6 3]] 2 8) ;=> [[2 5 2] [4 8 2] [5 6 3]]
  "
  [m a b]
  (mp/element-clamp m a b))

(defn emap!
  "Element-wise map of a function f over all elements of one or more arrays.

   f must return a result compatible with the element-type of the array m

   Performs in-place modification of the first array argument."
  ([f m]
    (mp/element-map! m f) m)
  ([f m a]
    (mp/element-map! m f a) m)
  ([f m a & more]
    (mp/element-map! m f a more) m))

(defn add-emap!
  "Adds the result of emap to a destination array.

   dest must be a mutable numerical array. Returns dest."
  ([dest f a]
    (mp/add-emap! dest f a) dest)
  ([dest f a b]
    (mp/add-emap! dest f a b) dest)
  ([dest f a b & more]
    (mp/add-emap! dest f a b more) dest))

(defn set-emap!
  "Sets the destination array to the result of an emap operation.

   dest must be a mutable array. Returns dest."
  ([dest f a]
    (mp/set-emap! dest f a) dest)
  ([dest f a b]
    (mp/set-emap! dest f a b) dest)
  ([dest f a b & more]
    (mp/set-emap! dest f a b more) dest))

(defn emap-indexed!
  "Element-wise map-indexed over all elements of one or more arrays.

   f must accept as first argument the index vector of the current element,
   and return a result compatible with the element-type of the array m

   Performs in-place modification of the first array argument."
  ([f m]
    (mp/element-map-indexed! m f))
  ([f m a]
    (mp/element-map-indexed! m f a))
  ([f m a & more]
    (mp/element-map-indexed! m f a more)))

(defn index-seq-for-shape
  "Returns a sequence of all possible index vectors for a given shape, in row-major order"
  [sh]
  (u/base-index-seq-for-shape sh))

(defn index-seq
  "Returns a sequence of all possible index vectors into an array, in row-major order."
  [m]
  (index-seq-for-shape (mp/get-shape m)))

;; =========================================================
;; Print Matrix

#?(:clj
(defn pm
  "Pretty-prints a matrix or dataset. 

   Column names will be printed if they are defined. The option :column-names? can be used to turn 
   this on or off explicity.

   opts is a map of optional parameters which may include:

      :formatter     - a function to format each array element
      :column-names? - true to force printing of column names, false to supress"
  ([m]
    (println (pprint/pm m)))
  ([m opts]
    (println (pprint/pm m opts))))
)

;; =========================================================
;; Implementation management functions

(defn current-implementation
  "Gets the currently active matrix implementation as a keyword, e.g. :vectorz"
  ;;{:inline (fn [] imp/*matrix-implementation*)} ;; Seems to cause caching issues when setting implementations?
  ([] imp/*matrix-implementation*))

(defn- implementation-check
  "Gets a canonical matrix object for a given implementation (keyword or array), or the current implementation
   if not otherwise specified. Scalars are regarded as conforming to the current implementation
   Throws an exception if none is available."
  ([]
    (or
      (imp/get-canonical-object imp/*matrix-implementation*)
      (error "No current clojure.core.matrix implementation available (no canonical)")))
  ([impl]
    (if-let [im (imp/get-canonical-object impl)]
      im
      (cond
        ;; for scalars, we default to using the current matrix implementation
        (scalar? impl) (imp/get-canonical-object imp/*matrix-implementation*)
        :else (or
                (imp/load-implementation impl)
                (error "No clojure.core.matrix implementation available - " (str impl)))))))

(defn current-implementation-object
  "Gets a canonical object for the currently active matrix implementation. This object
   can be used to pass as an implementation parameter, or to query implementation internals via core.matrix protocols."
  ([] (imp/get-canonical-object (current-implementation))))

(defn set-current-implementation
  "Sets the currently active core.matrix implementation.

   Parameter may be
    - A known keyword for the implementation e.g. :vectorz
    - An existing instance from the implementation

   Throws an exception if the implementation cannot be loaded.

   This is used primarily for functions that construct new matrices, i.e. it determines the
   implementation used for expressions like: (matrix [[1 2] [3 4]])"
  ([m]
    (imp/set-current-implementation m)))
