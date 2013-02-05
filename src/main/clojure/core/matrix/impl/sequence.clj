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


(extend-protocol mp/PSliceView
  clojure.lang.ISeq
    (get-major-slice-view [m i] (nth m i)))

(extend-protocol mp/PSliceSeq
  clojure.lang.ISeq
    (get-major-slice-seq [m] m))

(extend-protocol mp/PDimensionInfo
  clojure.lang.ISeq
    (dimensionality [m]
      (inc (mp/dimensionality (first m))))
    (is-vector? [m]
      (== 0 (mp/dimensionality (first m))))
    (is-scalar? [m]
      false)
    (get-shape [m]
      (cons (count m) (mp/get-shape (first m))))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (mp/dimension-count (first m) (dec x)))))

(extend-protocol mp/PFunctionalOperations
  clojure.lang.ISeq
    (element-seq [m]
      (mapcat mp/element-seq m))
    (element-map
      ([m f]
        (map #(mp/element-map % f) m))
      ([m f a]
        (map #(mp/element-map % f %2) m a))
      ([m f a more]
        (map #(mp/element-map % f %2 %3) m a more)))
    (element-map!
      ([m f]
        (if (== 1 (mp/dimensionality m))
          (error "Sequence arrays are not mutable!")
          (doseq [s m] (mp/element-map! s f))))
      ([m f a]
        (error "Sequence arrays are not mutable!"))
      ([m f a more]
        (error "Sequence arrays are not mutable!")))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m)))))

;; =====================================
;; Register implementation

(imp/register-implementation '())