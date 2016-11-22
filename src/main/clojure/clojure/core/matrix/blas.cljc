(ns clojure.core.matrix.blas
  "Namespace providing core.matrix implementations of standard BLAS operations.

   Operations may be optimised by underlying implementation: if not then 
   equivalent default core.matrix operations will be used."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.utils :as u]))
  
(defn gemm!
  "Implements the BLAS operation: result := alpha*A*B + beta*DEST.
   Optionally transposes matrices A and B. If beta is not provided, defaults to 0.0."
  ([alpha a b dest]
    (mp/gemm! dest false false alpha a b 0.0))
  ([alpha a b beta dest ]
    (mp/gemm! dest false false alpha a b beta))
  ([trans-a? trans-b? alpha a b beta dest ]
    (mp/gemm! dest trans-a? trans-b? alpha a b beta)))

(defn gemv! 
  "Implements the BLAS operation: result := alpha*A*B + beta*DEST
   Optionally transposes matrix A. If beta is not provided, defaults to 0.0."
  ([alpha a b dest]
    (mp/gemv! dest false alpha a b 0.0))
  ([alpha a b beta dest]
    (mp/gemv! dest false alpha a b beta))
  ([trans-a? alpha a b beta dest]
    (mp/gemv! dest trans-a? alpha a b beta)))

(defn gemm
  "Implements the BLAS operation: result := alpha*A*B + beta*C.
   Optionally transposes matrices A and B. If beta is not provided, defaults to 0.0."
  ([alpha a b c]
    (let [dest (m/mutable c)]
      (mp/gemm! dest false false alpha a b 0.0)
      dest))
  ([alpha a b beta c]
    (let [dest (m/mutable c)]
      (mp/gemm! dest false false alpha a b beta)
      dest))
  ([trans-a? trans-b? alpha a b beta c]
    (let [dest (m/mutable c)]
      (mp/gemm! dest trans-a? trans-b? alpha a b beta)
      dest)))

(defn gemv 
  "Implements the BLAS operation: result := alpha*A*B + beta*C
   Optionally transposes matrix A. If beta is not provided, defaults to 0.0."
  ([alpha a b c]
    (let [dest (m/mutable c)]
      (mp/gemv! dest false alpha a b 0.0)
      dest))
  ([alpha a b beta c]
    (let [dest (m/mutable c)]
      (mp/gemv! dest false alpha a b beta)
      dest))
  ([trans-a? alpha a b beta c]
    (let [dest (m/mutable c)]
      (mp/gemv! dest trans-a? alpha a b beta)
      dest)))
