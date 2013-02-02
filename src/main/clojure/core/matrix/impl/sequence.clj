(ns core.matrix.impl.sequence
  (:require [core.matrix.protocols :as mp])
  (:use core.matrix.utils)
  (:require [core.matrix.implementations :as imp])
  (:require [core.matrix.impl.mathsops :as mops])
  (:require [core.matrix.multimethods :as mm]))

(extend-protocol mp/PImplementation
  clojure.lang.ISeq
    (implementation-key [m] :sequence)
    (new-vector [m length] (seq (repeat length 0.0)))
    (new-matrix [m rows columns] (seq (repeat rows (mp/new-vector m columns))))
    (new-matrix-nd [m dims]
      (if-let [dims (seq dims)]
        (seq (repeat (first dims) (mp/new-matrix-nd m (next dims))))
        0.0))
    (construct-matrix [m data]
      (cond
        (mp/is-scalar? data)
          data
        (>= (mp/dimensionality data) 1)
          (map #(mp/construct-matrix m %) (for [i (range (mp/dimension-count data 0))] (mp/get-major-slice data i)))
        (sequential? data)
          (map #(mp/construct-matrix m %) data)
        :default
          (error "Don't know how to construct matrix from: " (class data))))
    (supports-dimensionality? [m dims]
      true))

(extend-protocol mp/PIndexedAccess
  clojure.lang.ISeq
    (get-1d [m x]
      (nth m (int x)))
    (get-2d [m x y]
      (let [row (nth m (int x))]
        (mp/get-1d row y)))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (nth m (int (first indexes)))]
          (mp/get-nd m next-indexes))
        (nth m (int (first indexes))))))

(extend-protocol mp/PDimensionInfo
  clojure.lang.ISeq
    (dimensionality [m]
      (if (seq m)
        (inc (mp/dimensionality (first m)))
        1))
    (is-vector? [m]
      (or (not (seq m)) (== 0 (mp/dimensionality (first m)))))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (cons (count m) (if (seq m) nil (mp/get-shape (first m)))))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (mp/dimension-count (m 0) (dec x)))))