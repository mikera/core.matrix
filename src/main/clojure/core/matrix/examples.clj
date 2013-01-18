(ns core.matrix.examples
  (:use core.matrix)
  (:use core.matrix.operators)
  (:refer-clojure :exclude [* - +]))

;; set core core.matrix examples

(defn all-examples []
  ;; a matrix can be defined using a nested vector
  (def a (matrix [[2 0] [0 2]]))

  ;; a matrix can be a pure clojure value 
  a
  
  ;; core.matrix.operators overloads operators to work on matrices
  (* 2 a)
  
  ;; a range of mathematical functions are defined for matrices
  (sqrt a)  
  
  ;; you can get rows and columns of matrices individually
  (get-row a 0)
  
  ;; Java double arrays can be used as vectors
  ;;(* a (double-array [1 2]))
)

