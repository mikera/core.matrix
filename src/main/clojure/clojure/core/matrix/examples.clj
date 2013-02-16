(ns clojure.core.matrix.examples
  (:refer-clojure :exclude [* - +])
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators))

;; =============================================================
;; Set of clojure.core.matrix examples

(defn all-examples []
  ;; a matrix can be defined using a nested vector
  (def a (matrix [[2 0] [0 2]]))

  ;; a matrix can be a pure clojure value 
  a
  
  ;; clojure.core.matrix.operators overloads operators to work on matrices
  (* a a)
  
  ;; a range of mathematical functions are defined for matrices
  (sqrt a)  
  
  ;; you can get rows and columns of matrices individually
  (get-row a 0)
  
  ;; Java double arrays can be used as input vectors
  (* a (double-array [1 2]))
  
  ;; you can also modify double arrays in place - they are examples of mutable vectors
  (let [a (double-array [1 4 9])]
    (sqrt! a)
    (seq a))
  
  ;; you can coerce matrices between different types
  (coerce [] (double-array [1 2 3]))
  
  ;; scalars can be used in many places that you can use a matrix
  (* [1 2 3] 2)
  
  ;; operations on scalars alone behave as you would expect
  (* 1 2 3 4 5)
  
  ;; you can do various functional programming tricks with matrices too
  (emap inc [[1 2] [3 4]])
  
)

