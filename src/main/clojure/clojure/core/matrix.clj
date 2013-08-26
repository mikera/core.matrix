(ns clojure.core.matrix
  (:use [clojure.core.matrix.utils])
  (:require [clojure.core.matrix.impl default double-array ndarray persistent-vector wrappers sparse-map])
  (:require [clojure.core.matrix.impl sequence]) ;; TODO: figure out if we want this?
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.impl.pprint :as pprint])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops]))

;; ==================================================================================
;; clojure.core.matrix API namespace
;;
;; This is the public API for clojure.core.matrix
;;
;; General handling of operations is as follows:
;;
;; 1. user calls public AI function defined in clojure.core.matrix
;; 2. clojure.core.matrix function delegates to a protocol for the appropriate function
;;    with protocols as defined in the clojure.core.matrix.protocols namespace. In most cases
;;    clojure.core.matrix will try to delagate as quickly as possible to the implementation.
;; 3. The underlying matrix implementation implements the protocol to handle the API
;;    function call
;; 4. It's up to the implementation to decide what to do then
;; 5. If the implementation does not understand one or more parameters, then it is
;;    expected to call the multimethod version in clojure.core.matrix.multimethods as this
;;    will allow an alternative implementation to be found via multiple dispatch
;;
;; ==================================================================================

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =============================================================
;; matrix construction functions

(declare current-implementation)
(declare implementation-check)
(declare current-implementation-object)
(def ^:dynamic *matrix-implementation* :persistent-vector)

(defn matrix
  "Constructs a matrix from the given data.

   The data may be in one of the following forms:
   - A valid existing matrix
   - Nested sequences of scalar values, e.g. Clojure vectors
   - A sequence of slices, each of which must be valid matrix data

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*"
  ([data]
    (mp/construct-matrix (implementation-check) data))
  ([implementation data]
    (mp/construct-matrix (implementation-check implementation) data)))

(defn array
  "Constructs a new n-dimensional array from the given data.

   The data may be in one of the following forms:
   - A valid existing matrix
   - Nested sequences of scalar values, e.g. Clojure vectors
   - A sequence of slices, each of which must be valid matrix data

   If implementation is not specified, uses the current matrix library as specified
   in *matrix-implementation*"
  ([data]
    (mp/construct-matrix (implementation-check) data))
  ([implementation data]
    (mp/construct-matrix (implementation-check implementation) data)))

(defn new-vector
  "Constructs a new zero-filled numerical vector with the given length.
   New matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable vectors, then the new vector will be fully mutable."
  ([length]
    (mp/new-vector (implementation-check) length))
  ([implementation length]
    (mp/new-vector (implementation-check implementation) length)))

(defn new-matrix
  "Constructs a new zero-filled numerical matrix with the given dimensions.
   New matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable matrices, then the new matrix will be fully mutable."
  ([rows columns]
    (mp/new-matrix (implementation-check) rows columns))
  ([implementation rows columns]
    (mp/new-matrix (implementation-check implementation) rows columns)))

(defn new-array
  "Creates a new array with the given shape.
   New matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable matrices, then the new matrix will be fully mutable."
  ([shape]
    (mp/new-matrix-nd (implementation-check) shape))
  ([implementation shape]
    (mp/new-matrix-nd (implementation-check implementation) shape)))

(defn new-scalar-array
  "Returns a new scalar array containing the scalar value zero."
  ([]
    (new-scalar-array *matrix-implementation*))
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
  "Constucts a row matrix with the given data. The returned matrix is a 2D 1xN row matrix.

   The data must be either a valid existing vector or a sequence of scalar values."
  ([data]
    (mp/construct-matrix (implementation-check) (vector data)))
  ([implementation data]
    (mp/construct-matrix (implementation-check implementation) (vector data))))

(defn column-matrix
  "Constucts a column matrix with the given data. The returned matrix is a 2D Nx1 column matrix.

   The data must be either a valid existing vector or a sequence of scalar values."
  ([data]
    (mp/construct-matrix (implementation-check) (map vector data)))
  ([implementation data]
    (mp/construct-matrix (implementation-check implementation) (map vector data))))

(defn identity-matrix
  "Constructs a 2D identity matrix with the given number of rows"
  ([dims]
    (mp/identity-matrix (implementation-check) dims))
  ([implementation dims]
    (mp/identity-matrix (implementation-check implementation) dims)))

(defn mutable-matrix
  "Constructs a mutable copy of the given matrix.

   If the implementation does not support mutable matrices, will return a mutable array
   from another core.matrix implementation that supports either the same element type or a broader type."
  ([data]
    (or (mp/mutable-matrix data)
        (clojure.core.matrix.impl.ndarray/ndarray data)))
  ([data type]
    (mutable-matrix data) ;; TODO: support creation with specific element types
    ))

(defn diagonal-matrix
  "Constructs a 2D diagonal matrix with the given numerical values on the main diagonal.
   All off-diagonal elements will be zero.
   diagonal-values may be a vector or any Clojure sequence of values."
  ([diagonal-values]
    (mp/diagonal-matrix (current-implementation-object) diagonal-values))
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
  "Creates a sparse matrix with the given data. Sparse matrices are required to store
  a M*N matrix with E non-zero elements in approx O(M+N+E) space or less.

  Throws an exception if creation of a sparse matrix is not possible"
  ([data]
    (sparse-matrix (current-implementation-object) data))
  ([implementation data]
    (TODO)))

(defmacro with-implementation [impl & body]
  "Runs a set of expressions using a specified matrix implementation.

   Example:
     (with-implementation :vectorz
       (new-matrix 10 10))"
  `(binding [*matrix-implementation* (imp/get-canonical-object ~impl)]
     ~@body))

;; ======================================
;; Implementation details

(defn supports-dimensionality?
  "Returns true if the implementation for a given matrix supports a specific dimensionality, i.e.
   can natively create and manipulate matrices with the given number of dimensions"
  ([m dimension-count]
    (let [m (if (keyword? m) (imp/get-canonical-object m) m)]
      (mp/supports-dimensionality? m dimension-count))))

(defn supports-shape?
  "Returns true if the implementation supports creation of matrices with a specific shape."
  [m shape]
  (let [m (if (keyword? m) (imp/get-canonical-object m) m)]
    (mp/supports-dimensionality? m (count shape))))

;; ======================================
;; matrix assignment and copying

(defn assign!
  "Assigns a new value to an array. Sets the values of the target elementwise, broadcasting where necessary.
   Returns the mutated array."
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
  "Assigns a value elementwise to a given matrix, broadcasting to fill the whole matrix as necessary.
   Returns a new matrix, of the same shape and implementation type as the original."
  ([m a]
    (mp/broadcast (mp/coerce-param m a) (mp/get-shape m))))

(defn clone
  "Constructs a clone of the matrix, using the same implementation. This function is intended to
   allow safe defensive copying of matrices / vectors.

   Guarantees that:
   1. Mutating the returned matrix will not modify any other matrix (defensive copy)
   2. The returned matrix will be fully mutable, if the implementation supports mutable matrices.

   A matrix implementation which only provides immutable matrices may safely return the same matrix."
  ([m]
    (mp/clone m)))

(defn to-nested-vectors
  "Converts an array to an idiomatic, immutable nested Clojure vector format.

   The depth of nesting will be equal to the dimensionality of the array."
  ([m]
    (mp/convert-to-nested-vectors m)))

(defn scalar 
  "Coerces m to a scalar value. Result is guaranteed not to be an array.
   Will throw an exception if m is not zero-dimensional."
  ([m]
    (mp/get-0d m)))

;; ==============================
;; Matrix predicates and querying

(defn array?
  "Returns true if the parameter is an N-dimensional array, for any N>=0"
  ([m]
    (not (mp/is-scalar? m))))

(defn matrix?
  "Returns true if parameter is a valid matrix (dimensionality == 2)"
  ([m]
    (== (mp/dimensionality m) 2)))

(defn vec?
  "Returns true if the parameter is a vector (1-dimensional array)"
  ([m]
    (mp/is-vector? m)))

(defn scalar?
  "Returns true if the parameter is a scalar value (i.e. acceptable as matrix element value).
   A 0-d array containing a scalar is *not* itself a scalar value."
  ([m]
    (mp/is-scalar? m)))

(defn zero-dimensional?
  "Returns true if the parameter has zero dimensions. i.e. it is a 0-d array or a scalar value."
  [m]
  (== 0 (mp/dimensionality m)))

(defn element-type
  "Returns the class of elements that can be in the array. For example, a numerical array may return
   the class java.lang.Double."
  ([m]
    (mp/element-type m)))

(defn dimensionality
  "Returns the dimensionality of an array. The dimensionality is equal to 
   the number of dimensions in the array's shape."
  ([m]
    (mp/dimensionality m)))

(defn row-count
  "Returns the number of rows in a matrix or vector (array must be 1D or more)"
  ([m]
    (mp/dimension-count m 0)))

(defn column-count
  "Returns the number of columns in a matrix (array must be 2D or more)"
  ([m]
    (mp/dimension-count m 1)))

(defn dimension-count
  "Returns the size of the specified dimension in a matrix. Will throw an error if the matrix
   does not have the specified dimension."
  ([m dim]
    (mp/dimension-count m dim)))

(defn square?
  "Returns true if matrix is square (i.e. a 2D array with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. is 2D and has exactly one row)"
  ([m]
    (and (== (mp/dimensionality m) 2)
         (== 1 (mp/dimension-count m 0)))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. is 2D and has has exactly one column)"
  ([m]
    (and (== (mp/dimensionality m) 2)
         (== 1 (mp/dimension-count m 1)))))

(defn shape
  "Returns the shape of a matrix, i.e. the dimension sizes for all dimensions.

   The result will be a vector containing only integer index values, with a count
   equal to the dimensionality of the array."
  ([m]
    (vec (mp/get-shape m))))

(defn mutable?
  "Returns true if the matrix is mutable, i.e. supports setting of values"
  ([m]
    (and (satisfies? mp/PIndexedSetting m) (mp/is-mutable? m))))

(defn conforming?
  "Returns true if two arrays have a conforming shape. Two arrays are conforming if there
   exists a common shape that both can broadcast to. This is a requirement for element-wise
   operations to work correctly on two different-shaped arrays."
  ([a] true)
  ([a b] (not (nil? (broadcast-shape (mp/get-shape a) (mp/get-shape b))))))

(defn same-shape?
  "Returns true if the arrays have the identical shape, false otherwise"
  ([] true)
  ([m] true)
  ([m n]
    (or
      (identical? m n)
      (clojure.core.matrix.utils/same-shape-object? (mp/get-shape m) (mp/get-shape n))))
  ([m n & more]
    (and
      (same-shape? m n)
      (every? #(same-shape? m %) more))))

(defn numerical?
  "Returns true if the matrix is a valid numerical matrix (i.e. supports numerical core.matrix operations)."
  ([m]
    (mp/numerical? m))) 

;; =======================================
;; Conversions

(defn to-double-array
   "Returns a double array containing the values of a numerical array m in row-major order.
    If want-copy? is true, will guarantee a new double array (defensive copy).
    If want-copy? is false, will return the internal array used by m, or nil if not supported
    by the implementation.
    If want-copy? is not sepcified, will return either a copy or the internal array"
   ([m]
     (mp/to-double-array m))
   ([m want-copy?]
     (let [arr (mp/as-double-array m)]
       (if want-copy?
         (if arr (copy-double-array arr) (mp/to-double-array m))
         arr))))

;; =======================================
;; matrix access

(defn mget
  "Gets a scalar value from an array at the specified position. Supports any number of dimensions."
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
   Will throw an exception if the matrix is not mutable.
   Returns the modified matrix (it is guaranteed to return the same instance)"
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
  "Gets a row of a matrix as a vector.
   Will return a mutable view if supported by the implementation."
  ([m x]
    (mp/get-row m x)))

(defn get-column
  "Gets a column of a matrix as a vector.
   Will return a mutable view if supported by the implementation."
  ([m y]
    (mp/get-column m y)))

(defn coerce
  "Coerces param into a format preferred by a specific matrix implementation.
   If param is already in a format deemed usable by the implementation, returns it unchanged."
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
    (mp/submatrix (list row-start row-length) (list col-start col-length))))

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
  "Gets a sequence of slices of a matrix. If dimension is supplied, slices along a given dimension,
   otherwise slices along the first dimension."
  ([m]
    (mp/get-major-slice-seq m))
  ([m dimension]
    (map #(mp/get-slice m dimension %) (range (mp/dimension-count m dimension)))))

(defn rows
  "Gets the rows of a matrix, as a sequence"
  ([m]
    (slices m)))

(defn columns
  "Gets the columns of a matrix, as a sequence"
  ([m]
    (slices m 1)))

(defn main-diagonal
  "Returns the main diagonal of a matrix or general array, as a vector. 
   The main diagonal of a general array is defined as those elements where the all the 
   indexes are equal, i.e. the index is of the form [i i ... i]"
  ([m]
    (mp/main-diagonal m)))

(defn diagonal 
  "Returns the specified diagonal of a 2D matrix as a vector.
   If k>0, returns a diagonal above the main diagonal.
   If k<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices."
  ([m] 
    (mp/main-diagonal [m]))
  ([m k]
    (TODO)))

(defn join
  "Joins arrays together, along dimension 0. Other dimensions must be compatible"
  ([& arrays]
    (reduce mp/join arrays)))

(defn join-along
  "Joins arrays together, along a specified dimension. Other dimensions must be compatible."
  ([dimension & arrays]
    (if (== 0 dimension)
      (apply join arrays)
      (TODO))))

(defn rotate
  "Rotates an array along specified dimensions."
  ([m dimension shift-amount]
    (let [c (mp/dimension-count m dimension)
          sh (mod shift-amount c)]
      (join-along dimension (submatrix m dimension [sh (- c sh)]) (submatrix m dimension [0 sh]))))
  ([m [shifts]]
    (TODO)))

(defn as-vector
  "Creates a view of an array as a single flattened vector. 
   Returns nil if this is not supported by the implementation."
  ([m]
    (mp/as-vector m)))

(defn to-vector
  "Creates a new array representing the elements of array m as a single flattened vector.
   The new vector will be a mutable copy if the implementation supports mutable vectors."
  ([m]
    (or
      (mp/to-vector m)
      (new-vector m (mp/element-seq m)))))


;; ====================================
;; structural change operations

(defn broadcast
  "Broadcasts a matrix to a specified shape. Returns a new matrix with the shape specified.
   The broadcasted matrix may be a view over the original matrix: attempting to modify the
   broadcasted matrix therefore has undefined results.
   Will throw an excption if broadcast to the target shape is not possible."
  ([m shape]
    (or (mp/broadcast m shape)
        (error "Broadcast to target shape: " (seq shape) " not possble."))))

(defn transpose
  "Transposes a matrix, returning a new matrix. For 2D matices, rows and columns are swapped.
   More generally, the dimension indices are reversed for any shape of array. Note that 1D vectors
   and scalars will be returned unchanged."
  ([m]
    (mp/transpose m)))

(defn transpose!
  "Transposes a square 2D matrix in-place. Will throw an exception if not possible."
  ([m]
    ;; TODO: implement with a proper protocol
    (assign! m (transpose m))))

(defn reshape
  "Changes the shape of a matrix to the specified new shape. shape can be any sequence of dimension sizes.
   Preserves the row-major order of matrix elements."
  ([m shape]
    (mp/reshape m shape)))

(defn fill!
  "Fills a matrix with a single scalar value. The scalar value must be compatible with the element-type
   of the array.

   Equivalent to assign!, but may be more efficient for scalar values."
  ([m value]
    (mp/fill! m value)
    m))

;; ======================================
;; matrix comparisons

(defn equals
  "Returns true if two arrays are numerically equal. 

   Will return false for arrays of different shapes.

   If epsilon is provided, performs an equality test
   with the given maximum tolerance (default is 0.0, i.e. exact numerical equivalence)"
  ([a b]
    (mp/matrix-equals a b))
  ([a b epsilon] ;; TODO: proper protocol implementation
    (every? #(<= (Math/abs (double %)) epsilon) (map - (mp/element-seq a) (mp/element-seq b)))))

;; ======================================
;; matrix maths / operations

(defn mul
  "Performs element-wise multiplication with numerical arrays."
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
  "Performs matrix multiplication (equivalent to inner-product)."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/matrix-multiply a b))
  ([a b & more]
    (reduce mp/matrix-multiply (mp/matrix-multiply a b) more)))

(defn e*
  "Element-wise multiply operator. Equivalent to emul."
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
  "Transforms a given vector with a matrix, returning a new vector."
  ([m v]
    (mp/vector-transform m v)))

(defn transform!
  "Transforms a given vector in place. Returns the transformed vector."
  ([m v]
    (mp/vector-transform! m v)
    v))

(defn add
  "Performs element-wise addition on one or more numerical arrays."
  ([a] a)
  ([a b]
    (mp/matrix-add a b))
  ([a b & more]
    (reduce mp/matrix-add (mp/matrix-add a b) more)))

(defn add-product
  "Adds the element-wise product of two numerical ararys to the first array.
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
  "Adds the product of two numerical arrays scaled by a given factor to the first array"
  ([m a b factor]
    (mp/add-scaled-product m a b factor)))

(defn add-scaled-product!
  "Adds the product of two numerical arrays scaled by a given factor to the first array. 
   Returns the mutated array."
  ([m a b factor]
    (mp/add-scaled-product! m a b factor)
    m))

(defn sub
  "Performs element-wise subtraction on one or more numerical arrays.
   Returns the first array after it has been mutated."
  ([a] (mp/negate a))
  ([a b]
    (mp/matrix-sub a b))
  ([a b & more]
    (reduce mp/matrix-sub (mp/matrix-sub a b) more)
    a))

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
  "Scales a numerical array by one or more scalar factors.
   Returns a new scaled matrix."
  ([m factor]
    (mp/scale m factor))
  ([m factor & more-factors]
    (mp/scale m (* factor (reduce * more-factors)))))

(defn scale!
  "Scales a numerical array by one or more scalar factors (in place).
   Returns the matrix after it has been mutated."
  ([m factor]
    (mp/scale! m factor)
    m)
  ([m factor & more-factors]
    (mp/scale! m (* factor (reduce * more-factors)))
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

(defn normalise-probabilities
  "Normalises a numerical probability vector, i.e. to a vector where all elements sum to 1.0.
   Negative values are clamped to 0.0. A zero vector will be set set to [1/n .... 1/n]."
  ([v]
    (let [v (mp/element-map v #(if (>= % 0.0) % 0.0))
          len (double (mp/element-sum v))]
      (cond
        (== len 1.0) v
        (== len 0.0) (coerce v (let [n (mp/dimension-count v 0)] (repeat n (/ 1.0 n))))
        :else (scale v (/ 1.0 len))))))

(defn normalise!
  "Normalises a numerical vector in-place (scales to unit length).
   Returns the modified vector."
  ([v]
    (mp/normalise! v)
    v))

(defn dot
  "Computes the dot product (1Dx1D inner product) of two numerical vectors"
  ([a b]
    (mp/vector-dot a b)))

(defn inner-product
  "Computes the inner product of numerical arrays.

   The inner product of two arrays with indexed dimensions {..i j} and {j k..} has dimensions {..i k..}. The inner-product of two vectors will be scalar."
  ([] 1.0)
  ([a]
    a)
  ([a b]
    (mp/inner-product a b))
  ([a b & more]
    (reduce inner-product (inner-product a b) more)))

(defn outer-product
  "Computes the outer product of numerical arrays."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/outer-product a b))
  ([a b & more]
    (reduce outer-product (outer-product a b) more)))

(defn cross
  "Computes the cross-product of two numerical 3D vectors"
  ([a b]
    (mp/cross-product a b)))

(defn cross!
  "Computes the cross-product of two numerical 3D vectors, storing the result in the first vector.
   Returns the (mutated) first vector."
  ([a b]
    (mp/cross-product! a b)
    a))

(defn distance
  "Calculates the euclidean distance between two numerical vectors."
  ([a b]
    (mp/distance a b)))

(defn det
  "Calculates the determinant of a 2D numerical matrix.

   Throws an exception if the matrix is not square."
  ([a]
    (mp/determinant a)))

(defn inverse
  "Calculates the inverse of a 2D numerical matrix."
  ([m]
    (mp/inverse m)))

(defn negate
  "Calculates the negation of a numerical array. Should normally be equivalent to scaling by -1.0"
  ([m]
    (mp/negate m)))

(defn negate!
  "Calculates the negation of a numerical array in place. Equivalent to scaling by -1.0"
  ([m]
    (mp/scale! m -1.0)))

(defn trace
  "Calculates the trace of a 2D numerical matrix (sum of elements on main diagonal)"
  ([a]
    (mp/trace a)))

(defn length
  "Calculates the euclidean length (magnitude) of a vector"
  ([m]
    (mp/length m)))

(defn length-squared
  "Calculates the squared length (squared magnitude) of a vector"
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

;; create all unary maths operators
(eval
  `(do ~@(map (fn [[name func]]
           `(defn ~name
              ([~'m]
                (~(symbol "clojure.core.matrix.protocols" (str name)) ~'m)))) mops/maths-ops)
     ~@(map (fn [[name func]]
           `(defn ~(symbol (str name "!"))
              ([~'m]
                (~(symbol "clojure.core.matrix.protocols" (str name "!")) ~'m)
                ~'m))) mops/maths-ops))
       )

;; ===================================
;; Linear algebra algorithms
;;

;; TODO: lu-decomposition etc.

(defn rank
  "Computes the rank of a matrix, i.e. the number of linearly independent rows"
  ([m]
    (mp/PMatrixRank m)))

(defn lu-decomposition
  "Computes the LU decompotion of a matrix. Returns a vector containing two matrices [L U]

   Intended usage: (let [[L U] (lu-decomosition M)] ....) "
  [m]
    (TODO))

(defn sv-decomposition
  "Computes the Singular Value decomposition of a matrix. Returns a vector containing three matrices [U S V*]

   Intended usage: (let [[U S V*] (sv-decomosition M)] ....)"
  [m]
    (TODO))

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix. Returns a vector containing two matrices [L L*]

   Intended usage: (let [[L L*] (cholesky-decomosition M)] ....)"
  [m]
    (TODO))

(defn qr-decomposition
  "Computes the QR decomposition of a matrix. Returns a vector containing two matrices [Q R]

   Intended usage: (let [[Q R] (qr-decomosition M)] ....)"
  [m]
    (TODO))

(defn eigen-decomposition
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a vector containing three matrices [Q A Qinv]

   A is a diagonal matrix whose diagonal elements are the eigenvalues.

   Intended usage: (let [[Q A Qinv] (eigen-decomosition M)] ....)"
  [m]
    (TODO))

;; ====================================
;; Functional operations
;;
;; these work like regular clojure seq, map, reduce etc. but operate on all elements of
;; a matrix in row-major ordering

(defn ecount
  "Returns the total count of elements in an array.

   Equal to the product of the lenegths of each dimension in the array's shape."
  ([m]
    (cond
      (array? m) (reduce *' 1 (mp/get-shape m))
      (scalar? m) 1
      :else (count m))))

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

(defn esum
  "Calculates the sum of all the elements in a numerical array"
  [m]
  (mp/element-sum m))

(defn e=
  "Returns true if all array elements are equal (using clojure.core/=).
   WARNING: a java.lang.Long does not equal a java.lang.Double.
   Use 'equals' or 'e==' instead if you want numerical equality."
  ([m1]
    true)
  ([m1 m2]
    (every? true? (map = (eseq m1) (eseq m2))))
  ([m1 m2 & more]
    (reduce (fn [r mi] (and r (e= m1 mi))) (e= m1 m2) more)))

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

(defn index-seq-for-shape [sh]
  "Returns a sequence of all possible index vectors for a given shape, in row-major order"
  (base-index-seq-for-shape sh))

(defn index-seq [m]
  "Returns a sequence of all possible index vectors into a matrix, in row-major order"
  (index-seq-for-shape (shape m)))

;; =========================================================
;; Print Matrix

(defn- longest-nums
  "Finds the longest string representation of
   a number in each column within a given matrix."
  [mat]
  (let [tmat (transpose mat)
        format-num #(format "%.3f" (double %))
        col-long #(reduce max (map count (map format-num %)))]
    (map col-long tmat)))

(defn- str-elem
  "Prints and element that takes up a given amount of whitespaces."
  [elem whitespaces]
  (let [formatter (str "%" whitespaces "." 3 "f")]
    (format formatter (double elem))))

(defn- str-row
  "Creates a string for each row with the desired
   amount of spaces between the elements."
  [[elem-head & elem-tail] [len-head & len-tail]] ;; the first element doesn't have a leading ws.
  (let [first-elem (str-elem elem-head len-head)
        body-elems (map str-elem elem-tail len-tail)]
  (str "[" first-elem " " (apply str body-elems) "]")))

(defn- rprint
  "Recursively prints each element with a leading
   line break and whitespace. If there are no
   elements left in the matrix it ends with a
   closing bracket."
  [[head & tail :as mat] acc len]
  (if (empty? mat)
    (str acc "]")
    (recur tail (str acc "\n " (str-row head len)) len)))

(defn pm
  "Pretty-prints a matrix"
  [m]
  (pprint/pm m))

;; =========================================================
;; Implementation management functions

(defn current-implementation
  "Gets the currently active matrix implementation (as a keyword)"
  ([] clojure.core.matrix/*matrix-implementation*))

(defn- implementation-check
  "Gets the currently active matrix implementation (as a matrix object). Throws an exception if none is available."
  ([]
    (if-let [ik clojure.core.matrix/*matrix-implementation*]
      (imp/get-canonical-object ik)
      (error "No current clojure.core.matrix implementation available")))
  ([impl]
    (if-let [im (imp/get-canonical-object impl)]
      im
      (error "No clojure.core.matrix implementation available - " (str impl)))))

(defn current-implementation-object
  "Gets the a canonical object for the currently active matrix implementation. This object
   can be used to pass as an implementation parameter, or to query implementation internals."
  ([] (imp/get-canonical-object (current-implementation))))

(defn set-current-implementation
  "Sets the currently active core.matrix implementation.

   This is used primarily for functions that construct new matrices, i.e. it determines the
   implementation used for expressions like: (matrix [[1 2] [3 4]])"
  ([m]
    (alter-var-root (var clojure.core.matrix/*matrix-implementation*)
                    (fn [_] (imp/get-implementation-key m)))))
