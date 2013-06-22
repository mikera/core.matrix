(ns clojure.core.matrix
  (:use [clojure.core.matrix.utils])
  (:require [clojure.core.matrix.impl default double-array ndarray persistent-vector wrappers sparse-map])
  (:require [clojure.core.matrix.impl sequence]) ;; TODO: figure out if we want this?
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.protocols :as mp])
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
  "Constructs a new zero-filled vector with the given length.
   New matrix will contain default values as defined by the implementation (usually null or zero).
   If the implementation supports mutable vectors, then the new vector will be fully mutable."
  ([length]
    (mp/new-vector (implementation-check) length))
  ([implementation length]
    (mp/new-vector (implementation-check implementation) length)))

(defn new-matrix
  "Constructs a new zero-filled matrix with the given dimensions. 
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
   from another core.matrix implementation that supports the same element type."
  ([data]
    (or (mp/mutable-matrix data) 
        (clojure.core.matrix.impl.ndarray/ndarray data)))) 

(defn diagonal-matrix
  "Constructs a 2D diagonal matrix with the given values on the main diagonal.
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
  "Assigns a value to a matrix.
   Returns the mutated matrix"
  ([m a]
    (mp/assign! m a)
    m))

(defn assign-array!
  "Assigns values to a matrix from an array.
   Returns the mutated matrix"
  ([m a]
    (mp/assign-array! m a)
    m))

(defn assign
  "Assigns a value to a matrix, broadcasting to fill the whole matrix as necessary.
   Returns a new matrix."
  ([m a]
    (mp/broadcast (mp/coerce-param m a) (mp/get-shape m)))) 

(defn clone
  "Constructs a clone of the matrix, using the same implementation. This function is intended to
   allow safe defensive copying of matrices / vectors.

   Guarantees that:
   1. Mutating the returned matrix will not modify any other matrix (defensive copy)
   2. The return matrix will be mutable, if the implementation supports mutable matrices.

   A matrix implementation which only provides immutable matrices may safely return the same matrix."
  ([m]
    (mp/clone m)))

(defn to-nested-vectors
  "Converts an array to nested vectors.
   The depth of nesting is equal to the dimensionality of the array."
  ([m]
    (mp/convert-to-nested-vectors m)))

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
  "Returns true if the parameter is a scalar value (i.e. zero dimensionality, acceptable as matrix value).
   A 0-d array containing a scalar is *not* itself a scalar value."
  ([m]
    (mp/is-scalar? m)))

(defn zero-dimensional?
  "Returns true if the parameter has zero dimensions. i.e. it is a 0-d array or a scalar value."
  [m]
  (== 0 (mp/dimensionality m)))

(defn element-type
  "Returns the class of elements in the array."
  ([m]
    (mp/element-type m))) 

(defn dimensionality
  "Returns the dimensionality (number of array dimensions) of a matrix / array"
  ([m]
    (mp/dimensionality m)))

(defn row-count
  "Returns the number of rows in a matrix or vector (must be 1D or more)"
  ([m]
    (mp/dimension-count m 0)))

(defn column-count
  "Returns the number of columns in a matrix (must be 2D or more)"
  ([m]
    (mp/dimension-count m 1)))

(defn dimension-count
  "Returns the size of the specified dimension in a matrix. Will throw an error if the matrix
   does not have the specified dimension."
  ([m dim]
    (mp/dimension-count m dim)))

(defn square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (mp/dimensionality m))
      (== (mp/dimension-count m 0) (mp/dimension-count m 1)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix (i.e. has exactly one row)"
  ([m]
    (and (== (mp/dimensionality m) 2)
         (== 1 (mp/dimension-count m 0)))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (i.e. has exactly one column)"
  ([m]
    (and (== (mp/dimensionality m) 2)
         (== 1 (mp/dimension-count m 1)))))

(defn shape
  "Returns the shape of a matrix, i.e. the dimension sizes for all dimensions.

   Result may be a sequence or Java array, to allow implemenations flexibility to return
   their own internal representation of matrix shape.

   You are guaranteed however that you can call `seq` on this to get a sequence of dimension sizes."
  ([m]
    (mp/get-shape m)))

(defn mutable?
  "Returns true if the matrix is mutable, i.e. supports setting of values"
  ([m]
    (and (satisfies? mp/PIndexedSetting m) (mp/is-mutable? m))))

(defn conforming?
  "Returns true if two matrices have a conforming shape. Two matrices are conforming if there
   exists a common shape that both can broadcast to." 
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

;; =======================================
;; Conversions

(defn to-double-array 
   "Returns a double array containing the values of m in row-major order. 
    If want-copy is true, will guarantee a new double array (defensive copy).
    If want-copy is false, will return the internal array used by m, or nil if not supported
    by the implementation.
    If want-copy is not sepcified, will return either a copy or the internal array"
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
  "Gets a scalar value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m]
    (mp/get-0d m))
  ([m x]
    (mp/get-1d m x))
  ([m x y]
    (mp/get-2d m x y))
  ([m x y & more]
    (mp/get-nd m (cons x (cons y more)))))

(defn mset
  "Sets a scalar value in a matrix at a specified position, returning a new matrix and leaving the
   original unchanged."
  ([m v] v)
  ([m x v]
    (mp/set-1d m x v))
  ([m x y v]
    (mp/set-2d m x y v))
  ([m x y z & more]
    (mp/set-nd m (cons x (cons y (cons z (butlast more)))) (last more))))

(defn mset!
  "Sets a scalar value in a matrix at a specified position. Supports any number of matrix dimensions.
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
  ([m param]
    (let [m (if (keyword? m) (imp/get-canonical-object m) m)]
      (or
        (mp/coerce-param m param)
        (mp/coerce-param m (mp/convert-to-nested-vectors param))))))

;; =====================================
;; matrix slicing and views

(defn submatrix
  "Gets a view of a submatrix, for a set of index-ranges.
   Index ranges should be  a sequence of [start, length] pairs.
   Index range pair can be nil (gets the whole range) "
  ([m index-ranges]
    (mp/submatrix m index-ranges))
  ([m dimension index-range]
    (mp/submatrix m (assoc (vec (repeat (mp/dimensionality m) nil)) dimension index-range))))

(defn subvector
  "Gets a view of part of a vector. The view maintains a reference to the original,
   so can be used to modify the original vector if it is mutable."
  ([m start length]
    (mp/subvector m start length)))

(defn slice
  "Gets a view of a slice of a matrix along a specific dimension.
   The returned matrix will have one less dimension.
   Slicing a 1D vector will return a scalar.

   Slicing on the first dimension (dimension 0) is likely to perform better
   for many matrix implementations."
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
  "Returns the main diagonal of a matrix or general array, as a vector"
  ([m]
    (mp/main-diagonal m)))

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
  "Rotates an array along specified dimensions"
  ([m dimension shift-amount]
    (let [c (mp/dimension-count m dimension)
          sh (mod shift-amount c)]
      (join-along dimension (submatrix m dimension [sh (- c sh)]) (submatrix m dimension [0 sh]))))
  ([m [shifts]]
    (TODO))) 

(defn as-vector
  "Creates a view of an array as a single flattened vector."
  ([m]
    (mp/as-vector m)))

(defn to-vector
  "Creates a new array representing the elements of array m as a single flattened vector"
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

;; ======================================
;; matrix comparisons

(defn equals
  "Returns true if two matrices are numerically equal. If epsilon is provided, performs an equality test
   with the given maximum tolerance (default is 0.0, i.e. exact numerical equivalence)"
  ([a b]
    (mp/matrix-equals a b))
  ([a b epsilon]
    (every? #(<= (Math/abs (double %)) epsilon) (map - (mp/element-seq a) (mp/element-seq b)))))

;; ======================================
;; matrix maths / operations

(defn mul
  "Performs standard matrix multiplication with matrices, vectors or scalars.

   Uses the inner product."
  ([] 1.0)
  ([a] a)
  ([a b]
    (cond
      (number? b) (if (number? a) (* a b) (mp/scale a b))
      (number? a) (mp/pre-scale b a)
      :else (or (mp/matrix-multiply a b) (mp/inner-product a b))))
  ([a b & more]
    (reduce mul (mul a b) more)))

(defn emul
  "Performs element-wise matrix multiplication. Matrices should be the same size."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce mp/element-multiply (mp/element-multiply a b) more)))

(defn e*
  "Matrix element-wise multiply operator. Equivalent to emul."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/element-multiply a b))
  ([a b & more]
    (reduce e* (e* a b) more)))

(defn div
  "Element-wise matrix division."
  ([a] (mp/element-divide a))
  ([a b] (mp/element-divide a b))
  ([a b & more] (reduce mp/element-divide (mp/element-divide a b) more))) 

(defn emul!
  "Performs in-place element-wise matrix multiplication."
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
  "Transforms a given vector, returning a new vector"
  ([m v] 
    (mp/vector-transform m v)))

(defn transform!
  "Transforms a given vector in place. Returns the transformed vector."
  ([m v] 
    (mp/vector-transform! m v)
    v))

(defn add
  "Performs element-wise matrix addition on one or more matrices."
  ([a] a)
  ([a b]
    (mp/matrix-add a b))
  ([a b & more]
    (reduce mp/matrix-add (mp/matrix-add a b) more)))

(defn add-product
  "Adds the product of two matrices to the first matrix"
  ([m a b]
    (mp/add-product m a b))) 

(defn add-product!
  "Adds the product of two matrices to the first matrix. Returns the mutated matrix."
  ([m a b]
    (mp/add-product! m a b)
    m)) 

(defn add-scaled
  "Adds a matrix scaled by a given factor to the first matrix"
  ([m a factor]
    (mp/add-scaled m a factor))) 

(defn add-scaled!
  "Adds a matrix scaled by a given factor to the first matrix. Returns the mutated matrix."
  ([m a factor]
    (mp/add-scaled! m a factor)
    m)) 

(defn add-scaled-product
  "Adds the product of two matrices scaled by a given factor to the first matrix"
  ([m a b factor]
    (mp/add-scaled-product m a b factor))) 

(defn add-scaled-product!
  "Adds the product of two matrices scaled by a given factor to the first matrix. Returns the mutated matrix."
  ([m a b factor]
    (mp/add-scaled-product! m a b factor)
    m)) 

(defn sub
  "Performs element-wise matrix subtraction on one or more matrices."
  ([a] a)
  ([a b]
    (mp/matrix-sub a b))
  ([a b & more]
    (reduce mp/matrix-sub (mp/matrix-sub a b) more)))

(defn add!
  "Performs element-wise mutable matrix addition on one or more matrices. 
   Returns the mutated matrix."
  ([a] a)
  ([a b]
    (mp/matrix-add! a b)
    a)
  ([a b & more]
    (reduce (fn [acc m] (add! acc m)) (add! a b) more)))

(defn sub!
  "Performs element-wise mutable matrix subtraction on one or more matrices. 
   Returns the mutated matrix."
  ([a] a)
  ([a b]
    (mp/matrix-sub! a b)
    a)
  ([a b & more]
    (reduce (fn [acc m] (sub! acc m)) (sub! a b) more)))

(defn scale
  "Scales a matrix by one or more scalar factors"
  ([m factor]
    (mp/scale m factor))
  ([m factor & more-factors]
    (mp/scale m (* factor (reduce * more-factors)))))

(defn scale!
  "Scales a matrix by one or more scalar factors (in place)"
  ([m factor]
    (mp/scale! m factor)
    m)
  ([m factor & more-factors]
    (mp/scale! m (* factor (reduce * more-factors)))
    m))

(defn square
  "Squares every element of a matrix."
  ([m]
    (e* m m))) ;; TODO: make this a protocol function

(defn normalise
  "Normalises a matrix (scales to unit length). 
   Returns a new normalised vector."
  ([v]
    (mp/normalise v)))

(defn normalise-probabilities
  "Normalises a probability vector, i.e. to a vector where all elements sum to 1"
  ([v]
    (let [v (mp/element-map v #(if (>= % 0.0) % 0.0))
          len (double (mp/element-sum v))]
      (cond 
        (== len 1.0) v
        (== len 0.0) (coerce v (let [n (mp/dimension-count v 0)] (repeat n (/ 1.0 n))))
        :else (scale v (/ 1.0 len)))))) 

(defn normalise!
  "Normalises a vector in-place (scales to unit length).
   Returns the modified vector."
  ([v]
    (mp/normalise! v)
    v))

(defn dot
  "Computes the dot product (inner product) of two vectors"
  ([a b]
    (mp/vector-dot a b)))

(defn inner-product 
  "Computes the inner product of two arrays. 

   The inner product of matrixes with indexed dimensions {.. i j} and {j k ...} has dimensions {.. i k ...}"
  ([] 1.0)
  ([a]
    a)
  ([a b]
    (mp/inner-product a b))
  ([a b & more]
    (reduce inner-product (inner-product a b) more)))

(defn outer-product 
  "Computes the outer product of matrices."
  ([] 1.0)
  ([a] a)
  ([a b]
    (mp/outer-product a b))
  ([a b & more]
    (reduce outer-product (outer-product a b) more))) 

(defn cross 
  "Computes the cross-product of two vectors"
  ([a b]
    (mp/cross-product a b))) 

(defn cross! 
  "Computes the cross-product of two vectors, storing the result in the first vector. 
   Returns the (mutated) first vector."
  ([a b]
    (mp/cross-product! a b)
    a))

(defn distance
  "Calculates the euclidean distance between two vectors."
  ([a b]
    (mp/distance a b)))

(defn det
  "Calculates the determinant of a matrix."
  ([a]
    (mp/determinant a)))

(defn inverse
  "Calculates the inverse of a matrix."
  ([m]
    (mp/inverse m))) 

(defn negate
  "Calculates the negation of a matrix. Should normally be equivalent to scaling by -1.0"
  ([m]
    (mp/negate m))) 

(defn negate!
  "Calculates the negation of a matrix in place. Equivalent to scaling by -1.0"
  ([m]
    (mp/scale! m -1.0))) 

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

(defn pow
  "Raises every element of a numerical matrix by the given exponent."
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
  "Returns the total count of elements in an array."
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
   Returns a new array of the same type and shape."
  ([f m]
    (mp/element-map m f))
  ([f m a]
    (mp/element-map m f a))
  ([f m a & more]
    (mp/element-map m f a more)))

(defn esum
  "Calculates the sum of all the elements"
  [m]
  (mp/element-sum m))

(defn e=
  "Returns true if all array elements are equal (using Object.equals).
   WARNING: a java.lang.Long does not equal a java.lang.Double.
   Use 'equals' or 'e==' instead if you want numerical equality."
  ([m1]
    true)
  ([m1 m2]
    (every? true? (map = (eseq m1) (eseq m2))))
  ([m1 m2 & more]
    (reduce (fn [r mi] (and r (e= m1 mi))) (e= m1 m2) more))) 

(defn e==
  "Returns true if all array elements are numerically equal (using ==). Throws an error if any elements
   of the arrays being compared are not numerical values."
  ([m1]
    true)
  ([m1 m2]
    (equals m1 m2))
  ([m1 m2 & more]
    (reduce equals (equals m1 m2) more))) 

(defn emap!
  "Element-wise map over all elements of one or more arrays.
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
  [[mat-first & mat-rest :as m]]
  (let [len (longest-nums m)
        start (str "[" (str-row mat-first len))
        out (str start (rprint mat-rest "" len))]
    (println out)))

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
  "Sets the currently active matrix implementation"
  ([m]
    (alter-var-root (var clojure.core.matrix/*matrix-implementation*)
                    (fn [_] (imp/get-implementation-key m)))))
