(ns clojure.core.matrix
  (:require [clojure.core.matrix.impl.default :as default]
            [clojure.core.matrix.impl double-array object-array persistent-vector index]
            [clojure.core.matrix.impl sequence] ;; TODO: figure out if we want this?
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.pprint :as pprint]
            [clojure.core.matrix.implementations :as imp :refer [*matrix-implementation*]]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.utils :as u :refer [TODO error]]))

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
;; 5. If the implementation does not understand one or more parameters, then it is
;;    expected to call the multimethod version in clojure.core.matrix.multimethods as this
;;    will allow an alternative implementation to be found via multiple dispatch.
;;
;; ==================================================================================

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =============================================================
;; matrix construction functions

(declare current-implementation)
(declare implementation-check)
(declare current-implementation-object)
(declare to-nested-vectors)

(defn matrix
  "Constructs a new n-dimensional matrix from the given numerical data.

   The data may be in one of the following forms:
   - A valid existing numerical array
   - Nested sequences of scalar values, e.g. Clojure vectors
   - A sequence of slices, each of which must be valid matrix data

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*"
  ([data]
    (or
      (mp/construct-matrix (implementation-check) data)
      (mp/coerce-param [] data)))
  ([implementation data]
    (or
      (mp/construct-matrix (implementation-check implementation) data)
      (mp/coerce-param [] data))))

(defn array
  "Constructs a new n-dimensional array from the given data.

   The data may be in one of the following forms:
   - A valid existing array (which will be converted to the implementation)
   - Nested sequences of scalar values, e.g. Clojure vectors (must have regular shape)
   - A sequence of slices, each of which must be valid array data
   - A single scalar value, which will be wrapped or coerced as necessary for the implementation

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*

   If the implementation does not support the shape of data provided, may either
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

(defn index
  "Constructs a new index from given data.

   The data may be in one of the following forms:
   - A valid existing index
   - A 1D array of integer values
   - A sequence of integer values

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*

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
  "Constructs a new zero-filled numerical matrix with the given dimensions.

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
    (mp/new-matrix-nd (implementation-check) shape))
  ([implementation shape]
    (mp/new-matrix-nd (implementation-check implementation) shape)))

(defn new-array
  "Creates a new array with the given shape.
   New array will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable matrices, then the new matrix will be fully mutable."
  ([shape]
    (mp/new-matrix-nd (implementation-check) shape))
  ([implementation shape]
    (or (mp/new-matrix-nd (implementation-check implementation) shape)
        (mp/new-matrix-nd (implementation-check) shape)
        (mp/new-matrix-nd :persistent-vector shape)))) ;; todo: what is the right default?

(defn new-sparse-array
  "Creates a new sparse array with the given shape.
   New array will contain default values as defined by the implementation (usually zero).
   If the implementation supports mutable sparse matrices, then the new matrix will be fully mutable."
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

   If the implementation does not support mutable matrices, will return a mutable array
   from another core.matrix implementation that supports either the same element type or a broader type."
  ([data]
    (or (mp/mutable-matrix data)
        (default/construct-mutable-matrix data)))
  ([data type]
    (or (mp/mutable-matrix data)
        (default/construct-mutable-matrix data))
    ;; TODO: support creation with specific element types
    ))

(defn immutable
  "Returns an immutable array containing the given array data.

   May return the same array if it is already immutable.

   If the implementation does not support immutable matrices, will return an immutable array
   from another core.matrix implementation that supports either the same element type or a broader type."
  ([data]
    (or (mp/immutable-matrix data)
        (to-nested-vectors data)))
  ([data type]
    (or (mp/immutable-matrix data)
        (to-nested-vectors data))))

(defn ^{:deprecated true} mutable-matrix
  "Constructs a mutable copy of the given matrix.

   DEPRECATED: please use mutable instead"
  ([data]
    (mutable data))
  ([data type]
    (mutable data type)))

(defn ensure-mutable
  "Checks if an array is mutable, and if not converts to a new mutable array. Guarantees
   that the result will be mutable, but may not be the same type as the original array."
  ([m]
    (if (mp/is-mutable? m)
      m
      (mutable m))))

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
  "Creates a matrix with the specified shape, and each element specified by (f i j k...)
   Where i, j, k... are the index positions of each element in the matrix"
  ([shape f]
    (compute-matrix (implementation-check) shape f))
  ([implementation shape f]
    (let [m (implementation-check implementation)]
      (mp/compute-matrix m shape f))))

(defn sparse-matrix
  "Creates a sparse matrix with the given data, using a specified implementation
  or the current implementation if not specified. Sparse matrices are required to store
  a M*N matrix with E non-zero elements in approx O(M+N+E) space or less.

  Throws an exception if creation of a sparse matrix is not possible"
  ([data]
    (sparse-matrix (implementation-check) data))
  ([implementation data]
    (or (mp/sparse-coerce implementation data)
        (error "Sparse implementation not available"))))

(defn sparse-array
  "Creates a sparse array with the given data, using a specified implementation
  or the current implementation if not specified. 

  Throws an exception if creation of a sparse array is not possible"
  ([data]
    (sparse-matrix (implementation-check) data))
  ([implementation data]
    (or (mp/sparse-coerce implementation data)
        (error "Sparse implementation not available"))))

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
   allocate contiguous storage space for all elements.

   'dense' should not be used with very large arrays, and may throw an OutOfMemoryError
    if the dense array is too large to fit in available memory.

   Returns the array unchanged if such coercion is not possible, or if the array is already dense."
  ([data]
    (mp/dense data))
  ([implementation data]
    (or (mp/dense-coerce implementation data) (mp/coerce-param implementation data))))

(defmacro with-implementation
  "Runs a set of expressions using a specified matrix implementation.

   Example:
     (with-implementation :vectorz
       (new-matrix 10 10))"
  [impl & body]
  `(binding [imp/*matrix-implementation* (imp/get-canonical-object ~impl)]
     ~@body))

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

(defn assign!
  "Assigns a new value to an array. Sets the values of the target element-wise, broadcasting where necessary.
   Returns the mutated array. The new value may be either a scalar or a array of compatible (maybe smaller) shape."
  ([m a]
    (mp/assign! m a)
    m))

(defn assign-array!
  "Assigns values to a core.matrix array from a Java array.
   Returns the mutated core.matrix array"
  ([m a]
    (mp/assign-array! m a)
    m))

(defn assign
  "Assigns array a element-wise, broadcasting to fill the whole shape of m.
   Returns a new matrix, of the same shape as the original m."
  ([m a]
    (mp/assign m a)))

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
   nested vectors will contain the element values.

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
  "Returns true if the parameter is an N-dimensional array, for any N>=0"
  {:inline (fn [m] `(not (mp/is-scalar? ~m)))}
  ([m]
    (not (mp/is-scalar? m))))

(defn matrix?
  "Returns true if parameter is a valid matrix (i.e. an array with dimensionality == 2)"
  ([m]
    (== (long (mp/dimensionality m)) 2)))

(defn vec?
  "Returns true if the parameter is a vector (1-dimensional array)"
  ([m]
    (mp/is-vector? m)))

(defn scalar?
  "Returns true if the parameter is a scalar value (i.e. acceptable as matrix element value).
   A 0-d array containing a scalar is *not* itself a scalar value."
  {:inline (fn [m] `(mp/is-scalar? ~m))}
  ([v]
    (mp/is-scalar? v)))

(defn zero-dimensional?
  "Returns true if the parameter has zero dimensions. i.e. it is a 0-d array or a scalar value."
  [m]
  (== 0 (long (mp/dimensionality m))))

(defn identity-matrix?
  "Returns true if the parameter is an identity-matrix"
  [m]
  (mp/identity-matrix? m))

(defn zero-matrix?
  "Returns true if all the elements of the parameter are zero."
  [m]
  (mp/zero-matrix? m))

(defn symmetric?
  "Returns true if the parameter is a symmetric matrix"
  [m]
  (mp/symmetric? m))

(defn diagonal?
  "Returns true if the parameter is a diagonal matrix"
  ([m]
     (mp/diagonal? m)))

(defn upper-triangular?
  "Returns true if the parameter is a upper triangular matrix"
  ([m]
     (mp/upper-triangular? m)))

(defn lower-triangular?
  "Returns true if the parameter is a lower triangular matrix"
  ([m]
     (mp/lower-triangular? m)))

(defn orthogonal?
  "Returns true if the parameter is an orthogonal matrix"
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
  ([m]
    (mp/element-type m)))

(defn dimensionality
  "Returns the dimensionality of an array. The dimensionality is equal to
   the number of dimensions in the array's shape."
  {:inline (fn ([m] `(mp/dimensionality ~m)))}
  ([m]
    (mp/dimensionality m)))

(defn row-count
  "Returns the number of rows in a matrix or vector (array must be 1D or more)"
  {:inline (fn ([m] `(mp/dimension-count ~m 0)))}
  ([m]
    (mp/dimension-count m 0)))

(defn column-count
  "Returns the number of columns in a matrix (array must be 2D or more)"
  {:inline (fn ([m] `(mp/dimension-count ~m 1)))}
  ([m]
    (mp/dimension-count m 1)))

(defn dimension-count
  "Returns the size of the specified dimension in a matrix. Will throw an error if the matrix
   does not have the specified dimension."
  {:inline (fn ([m dim] `(mp/dimension-count ~m ~dim)))}
  ([m dim]
    (mp/dimension-count m dim)))

(defn slice-count
  "Returns the number of slices in an array (array must be 1D or more). The array is sliced
   in row-major order, i.e. this is the dimension count of the first dimension."
  {:inline (fn ([m] `(mp/dimension-count ~m 0)))}
  ([m]
    (mp/dimension-count m 0)))

(defn square?
  "Returns true if matrix is square (i.e. a 2D array with same number of rows and columns)"
  ([m]
    (and
      (== 2 (long (mp/dimensionality m)))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. is 2D and has exactly one row)"
  ([m]
    (and (== (long (mp/dimensionality m)) 2)
         (== 1 (mp/dimension-count m 0)))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. is 2D and has has exactly one column)"
  ([m]
    (and (== (long (mp/dimensionality m)) 2)
         (== 1 (mp/dimension-count m 1)))))

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

(defn zero-count
  "Returns the number of zeros in an array."
  ([m]
    (mp/zero-count m)))

(defn density
  "Returns the density of the matrix, defined as the proportion of non-zero elements"
  ([m]
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
  "Sets a scalar value in an array at the specified position, returning a new matrix and leaving the
   original unchanged."
  ([m v]
    (mp/set-0d m v))
  ([m x v]
    (mp/set-1d m x v))
  ([m x y v]
    (mp/set-2d m x y v))
  ([m x y z & more]
    (mp/set-nd m (cons x (cons y (cons z (butlast more)))) (last more))))

(defn mset!
  "Sets a scalar value in an array at the specified position. Supports any number of dimensions.

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
  ([m x y z & more]
    (mp/set-nd! m (cons x (cons y (cons z (butlast more)))) (last more))
    m))

(defn get-row
  "Gets a row of a matrix, as a vector.
   Will return a mutable view if supported by the implementation."
  ([m x]
    (mp/get-row m x)))

(defn get-column
  "Gets a column of a matrix, as a vector.
   Will return a mutable view if supported by the implementation."
  ([m y]
     (mp/get-column m y)))

(defn- slice-dims
  "Strips all leading dimensions whole count is 1"
  [erg dims-to-slice]
  (let [shape (mp/get-shape erg)]
    (loop [erg erg ds dims-to-slice acc (long 0)]
      (if (seq ds)
        (if (first ds) ;;slice current dimension
          (recur (mp/get-slice erg acc 0) (rest ds) acc)
          (recur erg (rest ds) (inc acc)))
        erg))))

(defn- normalise-arg-with-slicing
  "maps number to [number] and :all to (range s) where s is the shape entry for
   the current dimension also returns if the current dimension has to be
   sliced after selecting"
  [arg s]
  (cond
   (= :all arg) [(range s) false]
   (number? arg) [[arg] true]
   :else [(mp/element-seq arg) false]))

(defn- normalise-arg
  "maps number to [number] and :all to (range s) where s is the shape entry for
   the current dimension"
  [arg s]
  (cond
   (= :all arg) (range s)
   (number? arg) [arg]
   :else (mp/element-seq arg)))

(defn select
  "Returns a view containing all elements in a which are at the positions
   of the Cartesian product of args. An argument can be:
    - a number - selects like [number] but drops the current dimension,
    - a 1-dimensional array of numbers
    - the keyword :all which is the same as the range of all valid indices.
   The number of args must match the dimensionality of a.

   Examples:
   (select [[1 2][3 4]] 0 0) ;=> 1
   (select [[1 2][3 4]] 0 :all) ;=> [1 2]
   (select [[1 2][3 4]] [0 1] [0]) ;=> [[1] [3]]
   (select [[1 2][3 4]] :all 0) ;=> [1 3]"
  [a & args]
  (let [res (map normalise-arg-with-slicing args (mp/get-shape a))
        normalized-args (map first res)
        dims-to-slice (map second res)]
    (slice-dims (mp/select a normalized-args) dims-to-slice)))

(defn select-indices
  "Returns a one-dimensional array of the elements which are at the specified
   indices. An index is a one-dimensional array which element-count matches the
   dimensionality of a. Examples:
   (select-indices [[1 2] [3 4]] [[0 0][1 1]]) ;=> [1 4]"
  [a indices]
  (mp/get-indices a indices))

(defn set-selection
  "Like select but sets the elements in the selection to the value of the final argument. 
   Leaves a unchanged and returns the modified array"
  [a & args]
  (let [sel-args (butlast args) 
        value (last args)]
    (mp/set-selection a (map normalise-arg sel-args (mp/get-shape a)) value)))

(defn set-selection!
  "Like set-selection but destructively modifies the array a in place"
  [a & args]
  (let [sel-args (butlast args) values (last args)]
    (assign! (mp/select a (map normalise-arg sel-args (mp/get-shape a))) values)
    a))

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

;; =====================================
;; matrix slicing and views

(defn submatrix
  "Gets a view of a submatrix, for a set of index ranges.
   Index ranges should be [start, length] pairs.
   Index range pairs can be nil (gets the whole range) "
  ([m index-ranges]
    (mp/submatrix m index-ranges))
  ([m dimension index-range]
    (mp/submatrix m (assoc (vec (repeat (mp/dimensionality m) nil)) dimension index-range)))
  ([m row-start row-length col-start col-length]
    (mp/submatrix m [(list row-start row-length) (list col-start col-length)])))

(defn subvector
  "Gets a view of part of a vector. The view maintains a reference to the original,
   so can be used to modify the original vector if it is mutable."
  ([m start length]
    (mp/subvector m start length)))

(defn slice
  "Gets a slice of an array along a specific dimension.
   The returned array will have one less dimension.

   Slicing a 1D vector will return a scalar.

   Slicing on the first dimension (dimension 0) is likely to perform better
   for many array implementations, and is therefore the default if no
   dimension is specified."
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

   The key difference between 'slices' and 'slice-views' is that 'slice-views' must always return views. In order 
   to ensure this behaviour on mutable 1-dimensioanal arrays, it must return a sequence of 0-dimensioanal arrays."
  ([m]
    (mp/get-major-slice-view-seq m))
  ([m dimension]
    (if (== 0 dimension)
      (slice-views m)
      (map #(mp/get-slice m dimension %) (range (mp/dimension-count m dimension))))))

(defn rows
  "Gets the rows of a matrix, as a sequence of 1D vectors.

   If the array has more than 2 dimensions, will return the rows from all slices in order."
  ([m]
    (mp/get-rows m)))

(defn columns
  "Gets the columns of a matrix, as a sequence of 1D vectors.

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
        (neg? k) (mp/main-diagonal (mp/submatrix m [[(- k) (+ (mp/dimension-count m 0) k)]
                                                    [0 (mp/dimension-count m 1)]]))
        (pos? k) (mp/main-diagonal (mp/submatrix m [[0 (mp/dimension-count m 0)]
                                                    [k (- (mp/dimension-count m 1) k)]]))
        :else   (mp/main-diagonal m)))))

(defn join
  "Joins arrays together, along dimension 0. For 1D vectors, this behaves as simple concatenation. 

   Other dimensions must be compatible. To join arrays along a different dimension, use 'join-along' instead."
  ([& arrays]
    (reduce mp/join arrays)))

(defn join-along
  "Joins arrays together, concatenating them along the specified dimension.

   Other dimensions must be compatible."
  ([dimension & arrays]
    (or
      (reduce #(mp/join-along %1 %2 dimension) arrays)
      (u/error "Failure to joins arrays"))))

(defn rotate
  "Rotates an array along specified dimensions. 

   Elements rotated off will re-appear at the other side. The shape of the array will not be modified."
  ([m dimension shift-amount]
    (mp/rotate m dimension shift-amount))
  ([m shifts]
    (mp/rotate-all m shifts)))

(defn shift
  "Shifts all elements of an array along specified dimensions, maintaining the shape of the array.
   New spaces shifted into the array are filled with the appropriate zero value."
  ([m dimension shift-amount]
    (mp/shift m dimension shift-amount))
  ([m shifts]
    (mp/shift-all m shifts)))

(defn order
  "Reorders slices of an array along a specified dimension. Re-orders along major dimension
   if no dimension is specified."
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
  "Creates a new array representing the elements of array m as a single flattened vector."
  ([m]
    (or
      (mp/to-vector m)
      (array m (mp/to-object-array m)))))

;; ====================================
;; structural change operations

(defn broadcast
  "Broadcasts a matrix to a specified shape. Returns a new matrix with the shape specified.
   The broadcasted matrix may be a view over the original matrix: attempting to modify the
   broadcasted matrix therefore has undefined results.
   Will throw an exception if broadcast to the target shape is not possible."
  ([m shape]
    (or (mp/broadcast m shape)
        (u/error "Broadcast to target shape: " (vec shape) " not possible."))))

(defn broadcast-like
  "Broadcasts the second matrix to the shape of the first. See 'broadcast'."
  {:inline (fn ([m a] `(mp/broadcast-like ~m ~a)))}
  ([m a]
    (mp/broadcast-like m a)))

(defn broadcast-coerce
  "Broadcasts and coerces the second matrix to the shape and type of the first.
   Equivalent to (coerce m (broadcast-like m a))."
  {:inline (fn ([m a] `(mp/broadcast-coerce ~m ~a)))}
  ([m a]
    (mp/broadcast-coerce m a)))

(defn transpose
  "Transposes a matrix, returning a new matrix. For 2D matrices, rows and columns are swapped.
   More generally, the dimension indices are reversed for any shape of array. Note that 1D vectors
   and scalars will be returned unchanged.

   If ordering is provided, will re-order dimensions according to the provided order."
  ([m]
    (mp/transpose m))
  ([m ordering]
    (TODO)))

(defn transpose!
  "Transposes a square 2D matrix in-place. Will throw an exception if not possible."
  ([m]
    (mp/transpose! m)
    m))

(defn reshape
  "Changes the shape of a matrix to the specified new shape. shape can be any sequence of dimension sizes.

   Preserves the row-major order of matrix elements. Truncates the sequence of elements if the shape is smaller
   than the original shape.

   Pads with default values (dependent on implementation - but normally zero) if the shape is larger."
  ([m shape]
    (mp/reshape m shape)))

(defn fill!
  "Fills a matrix with a single scalar value. The scalar value must be compatible with the element-type
   of the array.

   Equivalent to assign!, but may be more efficient for scalar values."
  ([m value]
    (mp/fill! m value)
    m))

(defn fill
  "Fills a matrix with a single scalar value. The scalar value must be compatible with the element-type
   of the array. Returns a new array."
  ([m value]
    (assign m value)))

;; ======================================
;; matrix comparisons

(defn equals
  "Returns true if two arrays are numerically equal.

   Will return false for arrays of different shapes.

   May either return false or throw an error if the arrays are not numerical.

   If epsilon is provided, performs an equality test
   with the given maximum tolerance (default is 0.0, i.e. exact numerical equivalence)"
  ([a b]
    (mp/matrix-equals a b))
  ([a b epsilon]
    (mp/matrix-equals-epsilon a b epsilon)))

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

;; ======================================
;; matrix maths / operations

(defn mul
  "Performs element-wise multiplication with scalars and numerical arrays.

   Behaves like clojure.core/* for scalar values."
  ([] 1.0)
  ([a] a)
  ([a b]
    (cond
      (number? b) (if (number? a) (* a b) (mp/scale a b))
      (number? a) (mp/pre-scale b a)
      :else (mp/element-multiply a b)))
  ([a b & more]
    (reduce mul (mul a b) more)))

(defn emul
  "Performs element-wise multiplication."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce mp/element-multiply (mp/element-multiply a b) more)))

(defn mmul
  "Performs matrix multiplication on matrices or vectors.  Equivalent to
  inner-product when applied to vectors.  Will treat a 1D vector roughly as a
  1xN matrix (row vector) when it's the first argument, or as an Nx1 matrix
  (column vector) when it's the second argument--except that the dimensionality
  of the result will be different from what it would be with matrix arguments."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/matrix-multiply a b))
  ([a b & more]
    (reduce mp/matrix-multiply (mp/matrix-multiply a b) more)))

(defn e*
  "An element-wise multiply operator equivalent to emul."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce e* (e* a b) more)))

(defn div
  "Performs element-wise matrix division for numerical arrays."
  ([a] (mp/element-divide a))
  ([a b] (mp/element-divide a b))
  ([a b & more] (reduce mp/element-divide (mp/element-divide a b) more)))

(defn div!
  "Performs in-place element-wise matrix division for numerical arrays."
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
  "Performs in-place element-wise multiplication of numerical arrays."
  ([a] a)
  ([a b]
    (mp/element-multiply! a b)
    a)
  ([a b & more]
    (mp/element-multiply! a b)
    (doseq [c more]
      (mp/element-multiply! a c))
    a))

(defn emul!
  "Performs in-place element-wise multiplication of numerical arrays."
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

   The transformation may be a 2D matrix, but other types of transformation are also supported
   e.g. affine transformations."
  ([t v]
    (mp/vector-transform t v)))

(defn transform!
  "Transforms a given vector in place. Returns the transformed vector.

   The transformation must map an n-dimensional vector to another n-dimensional vector, i.e.
   if it is a 2D matrix then it must have shape [n x n]."
  ([t v]
    (mp/vector-transform! t v)
    v))

(defn add
  "Performs element-wise addition on one or more numerical arrays."
  ([] 0.0)
  ([a] a)
  ([a b]
    (mp/matrix-add a b))
  ([a b & more]
    (reduce mp/matrix-add (mp/matrix-add a b) more)))

(defn add-product
  "Adds the element-wise product of two numerical arrays to the first array.
   Arrays must be the same shape."
  ([m a b]
    (mp/add-product m a b)))

(defn add-product!
  "Adds the product of two numerical arrays to the first array. Returns the mutated array."
  ([m a b]
    (mp/add-product! m a b)
    m))

(defn add-scaled
  "Adds a numerical array scaled by a given factor to the first array"
  ([m a factor]
    (mp/add-scaled m a factor)))

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

(defn sub
  "Performs element-wise subtraction on one or more numerical arrays.
   Returns a new array."
  ([a] (mp/negate a))
  ([a b]
    (mp/matrix-sub a b))
  ([a b & more]
    (reduce mp/matrix-sub (mp/matrix-sub a b) more)))

(defn add!
  "Performs element-wise mutable addition on one or more numerical arrays.
   Returns the first array after it has been mutated."
  ([a] a)
  ([a b]
    (mp/matrix-add! a b)
    a)
  ([a b & more]
    (mp/matrix-add! a b)
    (doseq [m more] (mp/matrix-add! a m))
    a))

(defn sub!
  "Performs element-wise mutable subtraction on one or more numerical arrays.
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
  "Scales a numerical array by one or more scalar factors (in place). The default implementation supports numerical arrays and
   numbers as scalar values, however matrix implementations may extend this to support other scalar types.

   Returns the matrix after it has been mutated."
  ([m factor]
    (mp/scale! m factor)
    m)
  ([m factor & more-factors]
    (mp/scale! m (mp/element-multiply factor (reduce mp/element-multiply more-factors)))
    m))

(defn square
  "Squares every element of a numerical array."
  ([m]
    (mp/square m)))

(defn normalise
  "Normalises a numerical vector (scales to unit length).
   Returns a new normalised vector."
  ([v]
    (mp/normalise v)))

(defn normalise!
  "Normalises a numerical vector in-place (scales to unit length).
   Returns the modified vector."
  ([v]
    (mp/normalise! v)
    v))

(defn dot
  "Computes the dot product (1Dx1D inner product) of two numerical vectors.
   
   If either argument is not a vector, should compute a higher dimensional inner product."
  ([a b]
    (or
      (mp/vector-dot a b) ;; this allows a optimised implementation of 'dot' for vectors, which should be faster
      (mp/inner-product a b))))

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

   The outer product of two arrays with indexed dimensions {i..j} and {j..k} has dimensions {i..j j..k}, i.e. the dimensioanlity will be the
   sum of the dimensionalities of the two arguments."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/outer-product a b))
  ([a b & more]
    (reduce outer-product (outer-product a b) more)))

(defn cross
  "Computes the 3D cross-product of two numerical vectors.

   Behavior on other types is undefined."
  ([a b]
    (mp/cross-product a b)))

(defn cross!
  "Computes the cross-product of two numerical 3D vectors, storing the result in the first vector.
   Returns the (mutated) first vector."
  ([a b]
    (mp/cross-product! a b)
    a))

(defn distance
  "Calculates the euclidean distance between two numerical vectors.
   This is equivalent to (norm 2 (sub a b)) but may be optimised by the underlying implementation."
  ([a b]
    (mp/distance a b)))

(defn det
  "Calculates the determinant of a 2D square numerical matrix.

   May throw an exception if the implementation does not support computation of determinants."
  ([a]
    (mp/determinant a)))

(defn inverse
  "Calculates the inverse of a 2D numerical matrix. 

   Returns nil if the matrix is singular. May throw an exception if the implementation does not support inverses."
  ([m]
    (mp/inverse m)))

(defn negate
  "Calculates the negation of a numerical array.
   Generally equivalent to (scale m -1.0)"
  ([m]
    (mp/negate m)))

(defn negate!
  "Calculates the negation of a numerical array in place. Equivalent to scaling by -1.0
   Generally equivalent to (scale! m -1.0)"
  ([m]
    (mp/scale! m -1.0)))

(defn trace
  "Calculates the trace of a 2D numerical matrix (sum of elements on main diagonal).

   The matrix need not be square."
  ([a]
    (mp/trace a)))

(defn length
  "Calculates the euclidean length (magnitude) of a numerical vector"
  ([m]
    (mp/length m)))

(defn length-squared
  "Calculates the squared length (squared magnitude) of a numerical vector"
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

;; create all unary maths operators
(eval
  `(do ~@(map (fn [[name func]]
           `(defn ~name
              ~(str "Computes the " name " function on all elements of an array, using double precision values. Returns a new array.")
              ([~'m]
                (~(symbol "clojure.core.matrix.protocols" (str name)) ~'m)))) mops/maths-ops)
     ~@(map (fn [[name func]]
           `(defn ~(symbol (str name "!"))
              ~(str "Computes the " name " function on all elements of an array, using double precision values. Mutates the array in-place.")
              ([~'m]
                (~(symbol "clojure.core.matrix.protocols" (str name "!")) ~'m)
                ~'m))) mops/maths-ops))
       )

;; ==================================
;; Elementary row operations
;;

(defn swap-rows
  "Swap row i with row j in a matrix, returning a new matrix"
  [m i j]
  (mp/swap-rows m i j))

(defn multiply-row
  "Multiply row i by a constant factor"
  [m i factor]
  (mp/multiply-row m i factor))

(defn add-row
  "Add a row j (optionally multiplied by a scalar factor) to a row i
   and replace row i with the result"
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
   sparse matrices - sometimes as fast as O(1)"
  ([m]
    ;; TODO fast protocol implementation?
    (- (mp/element-count m) (mp/zero-count m))))

(defn non-zero-indices
  "Gets the non-zero indices of an array.
   - For a 1D vector, returns an ordered index list.
   - For a higher dimensional array, returns the non-zero-indices for each slice in row-major order."
  ([m] (mp/non-zero-indices m)))

;; ====================================
;; Functional operations
;;
;; these work like regular clojure seq, map, reduce etc. but operate on all elements of
;; a matrix in row-major ordering

(defn ecount
  "Returns the total count of elements in an array.

   Equal to the product of the lengths of each dimension in the array's shape.

   Returns 1 for a zero-dimensional array or scalar."
  ([m]
    (mp/element-count m)))

(defn eseq
  "Returns all elements of an array as a sequence in row-major order"
  ([m]
    (mp/element-seq m)))

(defn ereduce
  "Element-wise reduce on all elements of an array."
  ([f m]
    (mp/element-reduce m f))
  ([f init m]
    (mp/element-reduce m f init)))

(defn emap
  "Element-wise map over all elements of one or more arrays.

   f must return a result compatible with the element-type of the array m

   Returns a new array of the same element-type and shape as the array m."
  ([f m]
    (mp/element-map m f))
  ([f m a]
    (mp/element-map m f a))
  ([f m a & more]
    (mp/element-map m f a more)))

(defn emap-indexed
  "Element-wise map-indexed over all elements of one or more arrays.

   f must accept as first argument the index vector of the current element,
   and return a result compatible with the element-type of the array m

   Returns a new array of the same element-type and shape as the array m."
  ([f m]
    (mp/element-map-indexed m f))
  ([f m a]
    (mp/element-map-indexed m f a))
  ([f m a & more]
    (mp/element-map-indexed m f a more)))

;(defn slice-map
;  "Maps a function over all all slices of an array"
;  ([f m]
;    (mp/slice-map m f)))

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

(defn e=
  "Returns true if all array elements are equal (using the semantics of clojure.core/=).
   WARNING: a java.lang.Long does not equal a java.lang.Double.
   Use 'equals' or 'e==' instead if you want numerical equality."
  ([m1]
    true)
  ([m1 m2]
    (mp/value-equals m1 m2))
  ([m1 m2 & more]
    (and
      (mp/value-equals m1 m2)
      (apply e= m2 more))))

(defn e==
  "Returns true if all array elements are numerically equal. Throws an error if any elements
   of the arrays being compared are not numerical values."
  ([m1]
    true)
  ([m1 m2]
    (equals m1 m2))
  ([m1 m2 & more]
    (reduce equals (equals m1 m2) more)))

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
  "Returns a sequence of all possible index vectors into a matrix, in row-major order"
  [m]
  (index-seq-for-shape (mp/get-shape m)))

;; =========================================================
;; Print Matrix

(defn pm
  "Pretty-prints a matrix.

   opts is a map of optional parameters which may include:

      :formatter - a function to format each array element"
  ([m]
    (println (pprint/pm m)))
  ([m opts]
    (println (pprint/pm m opts))))

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
      (u/error "No current clojure.core.matrix implementation available")))
  ([impl]
    (if-let [im (imp/get-canonical-object impl)]
      im
      (cond
        ;; for scalars, we default to using the current matrix implementation
        (scalar? impl) (imp/get-canonical-object imp/*matrix-implementation*)
        :else (or 
                (imp/load-implementation impl)
                (u/error "No clojure.core.matrix implementation available - " (str impl)))))))

(defn current-implementation-object
  "Gets a canonical object for the currently active matrix implementation. This object
   can be used to pass as an implementation parameter, or to query implementation internals via core.matrix protocols."
  ([] (imp/get-canonical-object (current-implementation))))

(defn set-current-implementation
  "Sets the currently active core.matrix implementation. Parameter may be either a keyword identifying the 
   implementation, or an array instance from the implementation.

   This is used primarily for functions that construct new matrices, i.e. it determines the
   implementation used for expressions like: (matrix [[1 2] [3 4]])"
  ([m]
    (imp/set-current-implementation m)))
