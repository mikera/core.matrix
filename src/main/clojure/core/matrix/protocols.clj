(ns core.matrix.protocols
  (:require [core.matrix.impl.mathsops :as mops]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


;; ================================================================
;; core.matrix API protocols
;;
;; Matrix implementations should extend these for full API support
;;
;; This namespace is intended for use by API implementers only
;; core.matrix users should not access these protocols directly
;;
;; ================================================================

;; ===================================================================================
;; MANDATORY PROTOCOLS
;;
;; A compliant core.matrix implementation must implement these. 
;; Otherwise things will fail.

(defprotocol PDimensionInfo
  "Protocol to return standard dimension information about a matrix. 
   dimensionality and dimension-count are mandatory for implementations"
  (dimensionality [m])
  (is-scalar? [m])
  (is-vector? [m])
  (dimension-count [m x]))

;; protocol arity overloads behave oddly, so different names used for simplicity
;; we provide fast paths for 1D and 2D access (common case)
(defprotocol PIndexedAccess
  "Protocol for indexed read access to matrices and vectors."
  (get-1d [m x])
  (get-2d [m x y])
  (get-nd [m indexes]))

(defprotocol PCoercion
  "Protocol to coerce a parameter to a format usable by a specific implementation. It is 
   up to the implementation to determine what parameter types they support. If the
   implementation is unable to perform coercion, it may return nil." 
  (coerce-param [m param]))

;; ===================================================================================
;; OPTTIONAL PROTOCOLS
;; 
;; implementations don't need to provide these since fallback default implementations
;; are provided. However, they should consider doing so for performance reasons

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply [m a])
  (scale [m a])
  (pre-scale [m a]))

(defprotocol PMatrixAdd
  "Protocol to support matrix addition on an arbitrary matrices of same size"
  (matrix-add [m a])
  (matrix-sub [m a]))

(defprotocol PVectorOps
  "Protocol to support common vector operations."
  (vector-dot [a b])
  (length [a])
  (length-squared [a])
  (normalise [a]))

(defprotocol PMatrixOps
  "Protocol to support common matrix operations"
  (trace [m])
  (determinant [m])
  (inverse [m])
  (negate [m])
  (transpose [m]))

;; code generation for protocol with unary mathematics operations defined in c.m.i.mathsops namespace
;; also generate in-place versions e.g. signum!
(eval
  `(defprotocol PMathsFunctions
  "Protocol to support mathematic functions applied element-wise to a matrix"
  ~@(map (fn [[name func]] `(~name [~'m])) mops/maths-ops)
  ~@(map (fn [[name func]] `(~(symbol (str name "!")) [~'m])) mops/maths-ops)))

(defprotocol PMatrixSlices
  "Protocol to support getting slices of a matrix"
  (get-row [m i])
  (get-column [m i]))


(defprotocol PFunctionalOperations
  "Protocol to allow functional-style operations on matrix elements."
  ;; note that protocols don't like variadic args, so we convert to regular args
  (element-seq [m])
  (element-map [m f]
               [m f more])
  (element-map! [m f]
                [m f more])
  (element-reduce [m f] [m f init]))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers."
  (convert-to-nested-vectors [m]))
