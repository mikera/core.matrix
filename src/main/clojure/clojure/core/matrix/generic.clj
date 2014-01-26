(ns clojure.core.matrix.generic
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.generic-protocols :as gmp])
  (:require [clojure.core.matrix.impl.generic-default])
  (:require [clojure.core.matrix.impl.default])
  (:require [clojure.core.matrix.implementations :as imp]))

;; Placeholder namespace for generic versions of core.matrix algorithms

;; generic element accessor functions
;; TODO: revisit these names?
(defn default-value
  "Returns the default value for the given array. Will normally be either nil or zero."
  ([impl]
     (mp/get-1d (mp/new-matrix-nd (imp/get-canonical-object impl) [1]) 0)))

(defrecord Specialisation
    [add mul div sub abs scalar? supports-inequality?
     zero one = < > >= <= sqrt])


(defn mul
  "Performs element-wise multiplication with numerical arrays."
  ([spez ] (:one spez))
  ([spez a] a)
  ([spez a b]
    (cond
      ((:scalar? spez) b) (if ((:scalar? spez) a) ((:mul spez) a b) (gmp/generic-scale a b spez))
      ((:scalar? spez) a) (gmp/generic-pre-scale b a spez)
      :else (gmp/generic-element-multiply a b spez)))
  ([spez a b & more]
     (reduce (partial mul spez) (mul spez a b) more)))

(defn emul
  "Performs element-wise multiplication."
  ([spez ] (:one spez))
  ([spez a] a)
  ([spez a b]
    (gmp/generic-element-multiply a b spez))
  ([spez a b & more]
     (reduce #(gmp/generic-element-multiply %1 %2 spez)
             (gmp/generic-element-multiply a b spez) more)))

(defn mmul
  "Performs matrix multiplication on matrices or vectors.  Equivalent to
  inner-product when applied to vectors.  Will treat a 1D vector roughly as a
  1xN matrix (row vector) when it's the first argument, or as an Nx1 matrix 
  (column vector) when it's the second argument--except that the dimensionality 
  of the result will be different from what it would be with matrix arguments."
  ([spez ] (:one spez))
  ([spez a] a)
  ([spez a b]
    (gmp/generic-matrix-multiply a b spez))
  ([spez a b & more]
     (reduce #(gmp/generic-matrix-multiply %1 %2 spez)
             (gmp/generic-matrix-multiply a b spez) more)))

(defn e*
  "Element-wise multiply operator. Equivalent to emul."
  ([spez ] (:one spez))
  ([spez a] a)
  ([spez a b]
    (gmp/generic-element-multiply a b spez))
  ([spez a b & more]
     (reduce (partial e* spez) (e* spez a b) more)))

(defn div
  "Performs element-wise matrix division for numerical arrays."
  ([spez a] (gmp/generic-element-divide a spez))
  ([spez a b] (gmp/generic-element-divide a b spez))
  ([spez a b & more] (reduce #(gmp/generic-element-divide %1 %2 spez)
                             (gmp/generic-element-divide a b spez) more)))

(defn div!
  "Performs in-place element-wise matrix division for numerical arrays."
  ([spez a]
     (gmp/generic-element-divide! a spez)
     a)
  ([spez a b]
     (gmp/generic-element-divide! a b spez)
     a)
  ([spez a b & more]
     (gmp/generic-element-divide! a b spez)
     (doseq [c more]
       (gmp/generic-element-divide! a c spez))
     a))

(defn mul!
  "Performs in-place element-wise multiplication of numerical arrays."
  ([spez a] a)
  ([spez a b]
    (gmp/generic-element-multiply! a b spez)
    a)
  ([spez a b & more]
    (gmp/generic-element-multiply! a b spez)
    (doseq [c more]
      (gmp/generic-element-multiply! a c spez))
    a))

(defn emul!
  "Performs in-place element-wise multiplication of numerical arrays."
  ([spez a] a)
  ([spez a b]
    (gmp/generic-element-multiply! a b spez)
    a)
  ([spez a b & more]
    (gmp/generic-element-multiply! a b spez)
    (doseq [c more]
      (gmp/generic-element-multiply! a c spez))
    a))

(defn add
  "Performs element-wise addition on one or more numerical arrays."
  ([spez a] a)
  ([spez a b]
    (gmp/generic-matrix-add a b spez))
  ([spez a b & more]
     (reduce #(gmp/generic-matrix-add %1 %2 spez)
             (gmp/generic-matrix-add a b spez) more)))

(defn add-product
  "Adds the element-wise product of two numerical ararys to the first array.
   Arrays must be the same shape."
  ([spez m a b]
    (gmp/generic-add-product m a b spez)))

(defn add-product!
  "Adds the product of two numerical arrays to the first array. Returns the mutated array."
  ([spez m a b]
    (gmp/generic-add-product! m a b spez)
    m))

(defn add-scaled
  "Adds a numerical array scaled by a given factor to the first array"
  ([spez m a factor]
    (gmp/generic-add-scaled m a factor spez)))

(defn add-scaled!
  "Adds a numerical array scaled by a given factor to the first array. Returns the mutated array."
  ([spez m a factor]
    (gmp/generic-add-scaled! m a factor spez)
    m))

(defn add-scaled-product
  "Adds the product of two numerical arrays scaled by a given factor to the first array"
  ([spez m a b factor]
    (gmp/generic-add-scaled-product m a b factor spez)))

(defn add-scaled-product!
  "Adds the product of two numerical arrays scaled by a given factor to the first array.
   Returns the mutated array."
  ([spez m a b factor]
    (gmp/generic-add-scaled-product! m a b factor spez)
    m))

(defn sub
  "Performs element-wise subtraction on one or more numerical arrays.
   Returns the first array after it has been mutated."
  ([spez a] (gmp/generic-negate a spez))
  ([spez a b]
    (gmp/generic-matrix-sub a b spez))
  ([spez a b & more]
     (reduce #(gmp/generic-matrix-sub %1 %2 spez)
             (gmp/generic-matrix-sub a b spez) more)))

(defn add!
  "Performs element-wise mutable addition on one or more numerical arrays.
   Returns the first array after it has been mutated."
  ([spez a] a)
  ([spez a b]
    (gmp/generic-matrix-add! a b spez)
    a)
  ([spez a b & more]
    (gmp/generic-matrix-add! a b spez)
    (doseq [m more] (gmp/generic-matrix-add! a m spez))
    a))

(defn sub!
  "Performs element-wise mutable subtraction on one or more numerical arrays.
   Returns the first array, after it has been mutated."
  ([spez a] a)
  ([spez a b]
    (gmp/generic-matrix-sub! a b spez)
    a)
  ([spez a b & more]
    (gmp/generic-matrix-sub! a b spez)
    (doseq [m more] (gmp/generic-matrix-sub! a m spez))
    a))

(defn scale
  "Scales a numerical array by one or more scalar factors.
   Returns a new scaled matrix."
  ([spez m factor]
    (gmp/generic-scale m factor spez))
  ([spez m factor & more-factors]
     (gmp/generic-scale m (gmp/generic-element-multiply factor (reduce #(gmp/generic-element-multiply %1 %2 spez) more-factors) spez) spez)))

(defn scale!
  "Scales a numerical array by one or more scalar factors (in place).
   Returns the matrix after it has been mutated."
  ([spez m factor]
    (gmp/generic-scale! m factor spez)
    m)
  ([spez m factor & more-factors]
     (gmp/generic-scale! m (gmp/generic-element-multiply factor (reduce #(gmp/generic-element-multiply %1 %2 spez) more-factors) spez) spez)
    m))

#_(defn square
  "Squares every element of a numerical array."
  ([spez m]
    (gmp/generic-square m spez)))

#_(defn normalise
  "Normalises a numerical vector (scales to unit length).
   Returns a new normalised vector."
  ([spez v]
    (gmp/generic-normalise v spez)))

#_(defn normalise!
  "Normalises a numerical vector in-place (scales to unit length).
   Returns the modified vector."
  ([spez v]
    (gmp/generic-normalise! v spez)
    v))

(defn dot
  "Computes the dot product (1Dx1D inner product) of two numerical vectors.

   If either argument is not a vector, computes a higher dimensional inner product."
  ([spez a b]
    (or 
      #_(gmp/generic-vector-dot a b spez)
      (gmp/generic-inner-product a b spez))))

(defn inner-product
  "Computes the inner product of numerical arrays.

   For matrix/matrix and matrix/vector arguments, this is equivalent to matrix multiplication.

   The inner product of two arrays with indexed dimensions {..i j} and {j k..} has dimensions {..i k..}. The inner-product of two vectors will be scalar."
  ([spez ] (:one spez))
  ([spez a]
    a)
  ([spez a b]
    (gmp/generic-inner-product a b spez))
  ([spez a b & more]
     (reduce #(gmp/generic-inner-product %1 %2 spez)
             (gmp/generic-inner-product a b spez) more)))

(defn outer-product
  "Computes the outer product of numerical arrays."
  ([spez ] (:one spez))
  ([spez a] a)
  ([spez a b]
    (gmp/generic-outer-product a b spez))
  ([spez a b & more]
     (reduce (partial outer-product spez) (outer-product spez a b) more)))

(defn distance
  "Calculates the euclidean distance between two numerical vectors."
  ([spez a b]
    (gmp/generic-distance a b spez)))


(defn negate
  "Calculates the negation of a numerical array. Should normally be equivalent to scaling by -1.0"
  ([spez m]
    (gmp/generic-negate m spez)))

(defn negate!
  "Calculates the negation of a numerical array in place. Equivalent to scaling by -1.0"
  ([spez m]
     (gmp/generic-scale! m ((:sub spez) (:one spez)) spez)))

(defn length
  "Calculates the euclidean length (magnitude) of a numerical vector"
  ([spez m]
    (gmp/generic-length m spez)))

(defn length-squared
  "Calculates the squared length (squared magnitude) of a numerical vector"
  ([spez m]
     (gmp/generic-length-squared m spez)))

