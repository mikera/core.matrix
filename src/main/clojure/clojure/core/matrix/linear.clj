(ns clojure.core.matrix.linear
  (:require [clojure.core.matrix.impl default])
  (:require [clojure.core.matrix.protocols :as mp]))


(defn qr
  "Computes QR decomposition of a matrix.
   Returns a map containing matrices of an input matrix type with the keys [:Q :R] such that:
        M = Q.R

   Where:
    - Q is an orthogonal matrix
    - R is an upper triangular matrix (= right triangular matrix)
   If :return parameter is specified in options map, it returns only specified keys.

   To provide a meaningful solution the original matrix must have full rank.

   Intended usage: (let [{:keys [Q R]} (qr M)] ....)
                   (let [{:keys [R]} (qr M {:return [:R]})] ....)"

  ([m options] (mp/qr m options))
  ([m] (mp/qr m {:return [:Q :R]})))
