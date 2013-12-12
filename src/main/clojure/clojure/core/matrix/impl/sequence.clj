(ns clojure.core.matrix.impl.sequence
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.multimethods :as mm]))

;; core.matrix implementation for Clojure ISeq objects
;;
;; Important notes:
;; 1. Intended mainly for accessing data. Not recommended for computations...
;; 2. generally returns a persistent vector where possible

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(extend-protocol mp/PImplementation
  clojure.lang.ISeq
    (implementation-key [m] :sequence)
    (meta-info [m]
      {:doc "Core.matrix implementation for Clojure ISeq objects"})
    (new-vector [m length] 
      (mp/new-vector [] length))
    (new-matrix [m rows columns] 
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m dims]
      (mp/new-matrix-nd [] dims))
    (construct-matrix [m data]
      (mp/coerce-param [] data))
    (supports-dimensionality? [m dims]
      true))

(extend-protocol mp/PIndexedAccess
  clojure.lang.ISeq
    (get-1d [m x]
      (scalar-coerce (nth m x)))
    (get-2d [m x y]
      (let [row (nth m x)]
        (mp/get-1d row y)))
    (get-nd [m indexes]
      (if-let [indexes (seq indexes)]
        (if-let [next-indexes (next indexes)]
          (let [mv (nth m (first indexes))]
            (mp/get-nd mv next-indexes))
          (nth m (first indexes)))
        m ;; TODO: figure out if this is a good return value? should it be an error?
        )))

(extend-protocol mp/PIndexedSetting
  clojure.lang.ISeq
    (set-1d [m row v]
      (mp/set-1d (mp/convert-to-nested-vectors m) row v))
    (set-2d [m row column v]
      (mp/set-2d (mp/convert-to-nested-vectors m) row column v))
    (set-nd [m indexes v]
      (mp/set-nd (mp/convert-to-nested-vectors m) indexes v))
    (is-mutable? [m]
      false))

(extend-protocol mp/PBroadcast
  clojure.lang.ISeq
    (broadcast [m new-shape]
      (mp/broadcast (mp/convert-to-nested-vectors m) new-shape)))

(extend-protocol mp/PBroadcastLike
  clojure.lang.ISeq
    (broadcast-like [m a]
      (mp/broadcast (mp/convert-to-nested-vectors a) (mp/get-shape m))))

(extend-protocol mp/PSliceView
  clojure.lang.ISeq
    (get-major-slice-view [m i]
      (nth m i)))

(extend-protocol mp/PSliceSeq
  clojure.lang.ISeq
    (get-major-slice-seq [m] m))

(extend-protocol mp/PConversion
  clojure.lang.ISeq
    (convert-to-nested-vectors [m]
      (if (> (mp/dimensionality (first m)) 0) 
        (mapv mp/convert-to-nested-vectors m)
        (mapv mp/get-0d m))))

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
        (mapv #(mp/element-map % f) m))
      ([m f a]
        (let [[m a] (mp/broadcast-compatible m a)]
          (mapv #(mp/element-map % f %2) m (mp/get-major-slice-seq a))))
      ([m f a more]
        (let [[m a & more] (apply mp/broadcast-compatible m a more)]
          (mapv #(mp/element-map % f %2 %3) m (mp/get-major-slice-seq a) (map mp/get-major-slice-seq more)))))
    (element-map!
      ([m f]
        (error "Sequence arrays are not mutable!"))
      ([m f a]
        (error "Sequence arrays are not mutable!"))
      ([m f a more]
        (error "Sequence arrays are not mutable!")))
    (element-reduce
      ([m f]
        (reduce f (mapcat mp/element-seq m)))
      ([m f init]
        (reduce f init (mapcat mp/element-seq m)))))

;; =====================================
;; Register implementation

(imp/register-implementation '())
