(ns clojure.core.matrix.demo.examples
  "Namespace providing some general core.matrix examples."
  (:refer-clojure :exclude [* - + == / < <= > >= not= = min max])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]))

;; =============================================================
;; Set of clojure.core.matrix examples

(defn all-examples []

  ;; =============================================================
  ;; MATRIX EXAMPLES
  ;; 
  ;; At its heart, core.matrix is all about namipulating arrays of
  ;; data. In many cases, these are 2D numerical matrices as commonly
  ;; used in mathematics and engineering.
  
  ;; a matrix can be defined using a nested vector of rows
  (def a (matrix [[2 0] [0 2]]))

  ;; a matrix is just a regular Clojure value
  a

  ;; clojure.core.matrix.operators overloads operators to work on matrices
  ;; example 1 : matrix x matrix element-wise multiplication
  (* a [[1 2] [3 4]])
  ; => [[2 0] [0 8]]
  
  ;; you can also do "matrix" multiplication with the `mmul` operation
  ;; note the difference compared to element-wise multiplication
  (mmul a [[1 2] [3 4]])
  ; => [[2.0 4.0] [6.0 8.0]]
  
  ;; a range of mathematical functions are defined for matrices
  ;; these return new matrices
  (sqrt a)

  ;; you can get rows and columns of matrices individually
  (get-row a 0)
  
  ;; various special matrix constructors are provided
  (identity-matrix 3)

  ;; pretty print a matrix
  (pm (matrix [[1 -1] [0.123 123]]))
  
  ;; you can do various "functional programming" operations with matrices
  ;; including emap, ereduce etc.
  (emap inc [[1 2] [3 4]])
  
  
  ;; =============================================================
  ;; VECTOR EXAMPLES
  ;; 
  ;; 1D numerical vectors are also commonly used types of array.
  ;; Many core.matrix operations are specially designed to woerk on 
  ;; vectors.
  
  ;; you can access parts of a vector with subvector
  (subvector [1 2 3 4 5 6] 2 3)

  ;; calculate the euclidean distance of two vectors
  (distance [0 0] [1 1])
  
  ;; =============================================================
  ;; SCALAR EXAMPLES
  ;; 
  ;; Scalars are individual values that can be used as elements of an array.
  ;; Usually these are numerical values (e.g. doubles) however core.matrix
  ;; does provide suport for non-numerical arrays.
  
  ;; scalar values can be used in many places that you can use a array
  ;; in these cases, scalars are "broadcast" to the shape of the target array
  (* [1 2 3] 2)
  ; => [2 4 6] 

  ;; operations on scalars alone behave as you would expect, i.e. exactly the
  ;; same as the equivalent clojure.core functions.
  (* 1 2 3 4 5)
  ; => 120 
  
  ;; scalars are considered to have `nil` shape
  (shape 1)
  ; => nil
  
  ;; you can also wrap scalars as 0-dimensional arrays. This usually isn't needed
  ;; but it allows for some extra functionality in some implementations 
  ;; (e.g. support for mutable arrays with a 0-dimensional shape)
  ;; If in doubt, it is best to avoid wrapping scalar values like this.
  (scalar-array 6)
  ; => #object[clojure.core.matrix.impl.wrappers.ScalarWrapper 0x2019c5a8 "6"]
  
  ;; =============================================================
  ;; INTEROP EXAMPLES
  ;; 
  ;; core.matrix is designed to allow easy interop with different data 
  ;; formats, including:
  ;;  - Java / host data types
  ;;  - Multiple matrix libaries
  ;;  - Regular Clojure data structures
  
  ;; Java double arrays can be used as 1D vectors
  (* a (double-array [1 2]))

  ;; you can also modify double arrays in place - they are examples of mutable vectors
  (let [v (double-array [1 4 9])]
    (sqrt! v)
    (vec v))  ;; use (vec ...) to display the contents of the array

  ;; you can coerce matrices between different formats. Use either an example of the desired
  ;; type or a keyword identifier to specify what type of matrix/vector you want.
  (coerce [] '(1 2 3 4))
  (coerce :double-array [1 2 3])



)

