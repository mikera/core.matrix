(ns clojure.core.matrix.generic-protocols
  (:require [clojure.core.matrix.impl.mathsops :as mops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defprotocol PGenericMatrixMultiply
  "generic version of clojure.core.matrix.protocols/PGenericMatrixMultiply"
  (generic-matrix-multiply [m a spec])
  (generic-element-multiply [m a spec]))

(defprotocol PGenericMatrixProducts
  "generic version of clojure.core.matrix.protocols/PGenericMatrixProducts"
  (generic-inner-product [m a spec])
  (generic-outer-product [m a spec]))

(defprotocol PGenericAddProduct
  "generic version of clojure.core.matrix.protocols/PGenericAddProduct"
  (generic-add-product [m a b spec]))

(defprotocol PGenericAddProductMutable
  "generic version of clojure.core.matrix.protocols/PGenericAddProductMutable"
  (generic-add-product! [m a b spec]))

(defprotocol PGenericAddScaledProduct
  "generic version of clojure.core.matrix.protocols/PGenericAddScaledProduct"
  (generic-add-scaled-product [m a b factor spec]))

(defprotocol PGenericAddScaledProductMutable
  "generic version of clojure.core.matrix.protocols/PGenericAddScaledProductMutable"
  (generic-add-scaled-product! [m a b factor spec]))

(defprotocol PGenericAddScaled
  "generic version of clojure.core.matrix.protocols/PGenericAddScaled"
  (generic-add-scaled [m a factor spec]))

(defprotocol PGenericAddScaledMutable
  "generic version of clojure.core.matrix.protocols/PGenericAddScaledMutable"
  (generic-add-scaled! [m a factor spec]))

(defprotocol PGenericMatrixDivide
  "generic version of clojure.core.matrix.protocols/PGenericMatrixDivide"
  (generic-element-divide
    [m spec]
    [m a spec]))

(defprotocol PGenericMatrixDivideMutable
  "generic version of clojure.core.matrix.protocols/PGenericMatrixDivideMutable"
  (generic-element-divide!
    [m spec]
    [m a spec]))

(defprotocol PGenericMatrixMultiplyMutable
  "generic version of clojure.core.matrix.protocols/PGenericMatrixMultiplyMutable"
  (generic-matrix-multiply! [m a spec])
  (generic-element-multiply! [m a spec]))

(defprotocol PGenericVectorTransform
  "generic version of clojure.core.matrix.protocols/PGenericVectorTransform"
  (generic-vector-transform [t v   spec] "Transforms a vector")
  (generic-vector-transform! [t v   spec] "Transforms a vector in place - mutates the vector argument"))

(defprotocol PGenericMatrixScaling
  "generic version of clojure.core.matrix.protocols/PGenericMatrixScaling"
  (generic-scale [m a spec]
    "Multiplies a array by the scalar value a, ")
  (generic-pre-scale [m a spec]
    "Pre-multiplies the array with the scalar. This is the same as scale for arrays
     where multiplication is commutative, but may be different for special kinds of scalars."))

(defprotocol PGenericMatrixMutableScaling
  "generic version of clojure.core.matrix.protocols/PGenericMatrixMutableScaling"
  (generic-scale! [m factor spec])
  (generic-pre-scale! [m factor spec]))

(defprotocol PGenericMatrixAdd
  "generic version of clojure.core.matrix.protocols/PGenericMatrixAdd"
  (generic-matrix-add [m a spec])
  (generic-matrix-sub [m a spec]))

(defprotocol PGenericMatrixAddMutable
  "generic version of clojure.core.matrix.protocols/PGenericMatrixAddMutable"
  (generic-matrix-add! [m a spec])
  (generic-matrix-sub! [m a spec]))

(defprotocol PGenericVectorOps
  "generic version of clojure.core.matrix.protocols/PGenericVectorOps"
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
  "generic version of clojure.core.matrix.protocols/PGenericVectorCross"
  (generic-cross-product [a b spec])
  (generic-cross-product! [a b spec]
    "Calculate cross product of two vectors, storing the result in the first vector"))

(defprotocol PGenericVectorDistance
  "generic version of clojure.core.matrix.protocols/PGenericVectorDistance"
  (generic-distance [a b spec]))

(defprotocol PGenericMutableVectorOps
  "generic version of clojure.core.matrix.protocols/PGenericMutableVectorOps"
  (generic-normalise! [a spec]))

(defprotocol PGenericMatrixOps
  "generic version of clojure.core.matrix.protocols/PMatrixOps"
  (generic-trace [m spec]
    "Returns the trace of a matrix (sum of elements on main diagonal.
     Must throw an error if the matrix is not square (i.e. all dimensions sizes are equal)")
  (generic-determinant [m spec]
    "Returns the determinant of a matrix.")
  (generic-inverse [m spec]
    "Returns the invese of a matrix. Should throw an exception if m is not invertible."))

(defprotocol PGenericNegation
  "generic version of clojure.core.matrix.protocols/PGenericNegation"
  (generic-negate [m spec]))


(eval
  `(defprotocol PGenericMathsFunctions
  "generic version of clojure.core.matrix.protocols/PGenericMathsFunctions"
  ~@(map (fn [[name func]] `(~(symbol (str "generic-" name)) [~'m ~'spec]))
      mops/maths-ops)))

(eval
  `(defprotocol PGenericMathsFunctionsMutable
  "generic version of clojure.core.matrix.protocols/PGenericMathsFunctionsMutable"
  ~@(map (fn [[name func]] `(~(symbol (str "generic-" name "!")) [~'m ~'spec]))
      mops/maths-ops)))


(defprotocol PGenericExponent
  "generic version of clojure.core.matrix.protocols/PGenericExponent"
  (generic-element-pow [m exponent spec]))

(defprotocol PGenericSquare
  "generic version of clojure.core.matrix.protocols/PGenericSquare"
  (generic-square [m spec]))


(defprotocol PGenericElementMinMax
  "generic version of clojure.core.matrix.protocols/PGenericElementMinMax"
  (generic-element-min [m spec])
  (generic-element-max [m spec])
  )

(defprotocol PGenericSummable
  "generic version of clojure.core.matrix.protocols/PGenericSummable"
  (generic-element-sum [m spec]))


(defprotocol PGenericValueEquality
  "generic version of clojure.core.matrix.protocols/PGenericValueEquality"
  (generic-value-equals [m a spec]))

(defprotocol PGenericMatrixEquality
  "generic version of clojure.core.matrix.protocols/PGenericMatrixEquality"
  (generic-matrix-equals [a b spec]
     "Return true if a equals b, i.e. if a and b are havee thesame shape and all elements are equal.
      Must use numerical value comparison on numbers (==) to account for matrices that may hold a mix of
      numercial types (e.g. java.lang.Long and java.lang.Double). Implementations that only support doubles
      should use Number.doubleValue() to get a numeric value to compare.
      May throw an exception if the matrices are non-numeric"))


(defprotocol PGenericMatrixEqualityEpsilon
  "generic version of clojure.core.matrix.protocols/PGenericMatrixEqualityEpsilon"
  (generic-matrix-equals-epsilon [a b eps spec]
    "As matrix-equals, but provides a numerical tolerance for equality testing."))



