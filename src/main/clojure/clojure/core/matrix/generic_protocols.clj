(ns clojure.core.matrix.generic-protocols)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defprotocol PGenericMatrixMultiply
  "Generic Version of PMatrixMultiply"
  (generic-matrix-multiply [m a spec])
  (generic-element-multiply [m a spec]))

(defprotocol PGenericMatrixProducts
  "Protocol for general inner and outer products of arrays.
   Products should use + and * as normally defined for numerical types"
  (genric-inner-product [m a spec])
  (generic-outer-product [m a spec]))


(defprotocol PGenericMatrixProducts
  "Generic Version of PMatrixMultiply"
  (generic-inner-product [m a spec])
  (generic-outer-product [m a spec]))

(defprotocol PGenericAddProduct
  "Protocol for add-product operation.
   Intended to implement a fast version for result = m + a * b"
  (generic-add-product [m a b spec]))

(defprotocol PGenericAddProductMutable
  "Protocol for mutable add-product! operation."
  (generic-add-product! [m a b spec]))

(defprotocol PGenericAddScaledProduct
  "Protocol for add-product operation.
   Intended to implement a fast version for result = m + a * b * factor"
  (generic-add-scaled-product [m a b factor spec]))

(defprotocol PGenericAddScaledProductMutable
  "Protocol for mutable add-product! operation."
  (generic-add-scaled-product! [m a b factor spec]))

(defprotocol PGenericAddScaled
  "Protocol for add-scaled operation.
   Intended to implement a fast version for result = m + a * factor"
  (generic-add-scaled [m a factor spec]))

(defprotocol PGenericAddScaledMutable
  "Protocol for mutable add-scaled! operation."
  (generic-add-scaled! [m a factor spec]))

(defprotocol PGenericMatrixDivide
  "Protocol to support element-wise division operator.
   One-arg version returns the reciprocal of all elements."
  (generic-element-divide
    [m spec]
    [m a spec]))

(defprotocol PGenericMatrixDivideMutable
  "Protocol to support mutable element-wise division operater.
   One-arg version returns the reciprocal of all elements."
  (generic-element-divide!
    [m spec]
    [m a spec]))

(defprotocol PGenericMatrixMultiplyMutable
  "Protocol to support mutable matrix multiplication on an arbitrary matrix, vector or scalar"
  (generic-matrix-multiply! [m a spec])
  (generic-element-multiply! [m a spec]))

(defprotocol PGenericVectorTransform
  "Protocol to support transformation of a vector to another vector.
   Is equivalent to matrix multiplication when 2D matrices are used as transformations.
   But other transformations are possible, e.g. affine transformations.

   A transformation need not be a core.matrix matrix: other types are permissible"
  (generic-vector-transform [t v   spec] "Transforms a vector")
  (generic-vector-transform! [t v   spec] "Transforms a vector in place - mutates the vector argument"))

(defprotocol PGenericMatrixScaling
  "Protocol to support numerical array scaling by scalar values. Provided because array classes may have
   efficient specialised scaling operaions.

   Works according the the default definition of multiplication for the matrix class
   (usually numerical, i.e. equivalent to clojure.core/+)"
  (generic-scale [m a spec]
    "Multiplies a array by the scalar value a, ")
  (generic-pre-scale [m a spec]
    "Pre-multiplies the array with the scalar. This is the same as scale for arrays
     where multiplication is commutative, but may be different for special kinds of scalars."))

(defprotocol PGenericMatrixMutableScaling
  "Protocol to support mutable array scaling by scalar values."
  (generic-scale! [m factor spec])
  (generic-pre-scale! [m factor spec]))

(defprotocol PGenericMatrixAdd
  "Protocol to support addition and subtraction on arbitrary matrices. 
   These are elementwise operations that should support broadcasting."
  (generic-matrix-add [m a spec])
  (generic-matrix-sub [m a spec]))

(defprotocol PGenericMatrixAddMutable
  "Protocol to support mutable addition and subtraction"
  (generic-matrix-add! [m a spec])
  (generic-matrix-sub! [m a spec]))


(defprotocol PGenericElementMinMax
  "Protocol to return the minimum and maximum elements in a numerical array. Must throw an exception 
   if the array is not numerical."
  (generic-element-min [m spec])
  (generic-element-max [m spec])
  )

(defprotocol PGenericSummable
  "Protocol to support the summing of all elements in an array.
   The array must hold numeric values only, or an exception will be thrown."
  (element-sum [m spec]))


(defprotocol PMatrixOps
  "Protocol to support common 2D numerical matrix operations"
  (trace [m spec]
    "Returns the trace of a matrix (sum of elements on main diagonal.
     Must throw an error if the matrix is not square (i.e. all dimensions sizes are equal)")
  (determinant [m spec]
    "Returns the determinant of a matrix.")
  (inverse [m spec]
    "Returns the invese of a matrix. Should throw an exception if m is not invertible."))

(defprotocol PGenericNegation
  (generic-negate [m spec]
    "Returns a new numerical array with all elements negated."))

(defprotocol PGenericValueEquality
  "Protocol for comparing two arrays, with the semantics of clojure.core/=.
   Returns false if the arrays are not of equal shape, or if any elements are not equal."
  (generic-value-equals [m a spec]))

(defprotocol PGenericMatrixEquality
  "Protocol for numerical array equality operations."
  (generic-matrix-equals [a b spec]
     "Return true if a equals b, i.e. if a and b are havee thesame shape and all elements are equal.
      Must use numerical value comparison on numbers (==) to account for matrices that may hold a mix of
      numercial types (e.g. java.lang.Long and java.lang.Double). Implementations that only support doubles
      should use Number.doubleValue() to get a numeric value to compare.
      May throw an exception if the matrices are non-numeric"))


(defprotocol PGenericMatrixEqualityEpsilon
  "Protocol for numerical array equality operations with a specified tolerance."
  (generic-matrix-equals-epsilon [a b eps spec]
    "As matrix-equals, but provides a numerical tolerance for equality testing."))


(defprotocol PGenericVectorOps
  "Protocol to support common numerical vector operations."
  (generic-vector-dot [a b spec]
     "Numerical dot product of two vectors. Must return a scalar value if the two parameters are 
      vectors of equal length.

      If the vectors are of unequal length, should throw an exception (however returning nil is
      also acceptable).

      Otherwise the implementation may optionally either return nil or compute a higher dimensional 
      inner-product (if it is able to do so).")
  (generic-length [a spec]
     "Euclidian length of a vector.")
  (generic-length-squared [a spec]
     "Squared Euclidean length of a vector.")
  (generic-normalise [a spec]
     "Returns a new vector, normalised to length 1.0"))

(defprotocol PGenericVectorCross
  (generic-cross-product [a b spec]
    "Cross product of two vectors")
  (generic-cross-product! [a b spec]
    "Calculate cross product of two vectors, storing the result in the first vector"))

(defprotocol PGenericVectorDistance
  (generic-distance [a b spec]
     "Euclidean distance of two vectors."))

(defprotocol PGenericMutableVectorOps
  "Protocol for mutable versions of common vector operations"
  (generic-normalise! [a spec]))

(defprotocol PGenericSummable
  "Protocol to support the summing of all elements in an array.
   The array must hold numeric values only, or an exception will be thrown."
  (generic-element-sum [m spec]))

(defprotocol PGenericExponent
  "Protocol to support the 'pow' function. Should raise every element of a matrix to a
   given exponent. Default implementation uses Java's Math/pow function which is appropriate for
   double values: arrays supporting arbitrary precision numbers or complex types will need to
   provide their own implementation."
  (generic-element-pow [m exponent spec]))

(defprotocol PGenericSquare
  "Protocol to support element-wise squaring of a numerical array."
  (generic-generic-square [m spec]))
