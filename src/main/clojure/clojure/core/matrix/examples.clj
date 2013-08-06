(ns clojure.core.matrix.examples
  (:refer-clojure :exclude [* - + == /])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators))

;; =============================================================
;; Set of clojure.core.matrix examples

(defn all-examples []
  ;; a matrix can be defined using a nested vector
  (def a (matrix [[2 0] [0 2]]))

  ;; a matrix is just a regular clojure value
  a

  ;; clojure.core.matrix.operators overloads operators to work on matrices
  ;; example: matrix x matrix multiplication
  (* a [[1 2] [3 4]])

  ;; a range of mathematical functions are defined for matrices
  ;; these return new matrices
  (sqrt a)

  ;; you can get rows and columns of matrices individually
  (get-row a 0)

  ;; Java double arrays can be used as input vectors
  (* a (double-array [1 2]))

  ;; you can also modify double arrays in place - they are examples of mutable vectors
  (let [v (double-array [1 4 9])]
    (sqrt! v)
    (vec v))  ;; use (vec ...) to display the contents of the array

  ;; you can coerce matrices between different formats. Use either an example of the desired
  ;; type or a keyword identifier to specify what type of matrix/vector you want.
  (coerce [] '(1 2 3 4))
  (coerce :double-array [1 2 3])

  ;; scalar values can be used in many places that you can use a matrix
  (* [1 2 3] 2)

  ;; operations on scalars alone behave as you would expect
  (* 1 2 3 4 5)

  ;; you can do various functional programming tricks with matrices too
  (emap inc [[1 2] [3 4]])

  ;; you can access parts of a vector with subvector
  (subvector [1 2 3 4 5 6] 2 3)

  ;; various special matrix constructors are provided
  (identity-matrix 3)

  ;; pretty print a matrix
  (pm (matrix [[1 -1] [0.123 123]]))

  ;; calculate the euclidean distance of two vectors
  (distance [0 0] [1 1])
)

