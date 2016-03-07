(ns clojure.core.matrix.blas
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :as u]))
  
(defn gemm!
  "Implements the BLAs operation: DEST := alpha*A*B + beta*DEST.
   Optionally transposes matrices A and B. If beta is not provided, defaults to 0.0."
  ([dest alpha a b]
    (mp/gemm! dest false false alpha a b 0.0))
  ([dest alpha a b beta]
    (mp/gemm! dest false false alpha a b beta))
  ([dest trans-a? trans-b? alpha a b beta]
    (mp/gemm! dest trans-a? trans-b? alpha a b beta)))

(defn gemv! 
  "Implements the BLAs operation: DEST := alpha*A*B + beta*DEST
   Optionally transposes matrix A. If beta is not provided, defaults to 0.0."
  ([dest alpha a b]
    (mp/gemv! dest false alpha a b 0.0))
  ([dest alpha a b beta]
    (mp/gemv! dest false alpha a b beta))
  ([dest trans-a? alpha a b beta]
    (mp/gemv! dest trans-a? alpha a b beta)))
