(ns clojure.core.matrix.impl.sequence
  "Namepace implementing selected core.matrix protocols for clojure sequences.

   Useful if you want to pass Clojure sequences directly to core.matrix functions.

   WARNING: because they lack efficient indexed access, sequences will perform badly for most
   array operations. In general they should be converted to other implementations before use."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
    #?(:clj [clojure.core.matrix.macros :refer [scalar-coerce error]]))
  #?(:clj (:import [clojure.lang ISeq])
     :cljs (:require-macros [clojure.core.matrix.macros :refer [scalar-coerce error]])))

;; core.matrix implementation for Clojure ISeq objects
;;
;; Important notes:
;; 1. Intended mainly for accessing data. Not recommended for computations...
;; 2. generally returns a persistent vector where possible

#?(:clj (do
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* true)
))

(extend-protocol mp/PImplementation
  ISeq
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
  ISeq
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
  ISeq
    (set-1d [m row v]
      (mp/set-1d (mp/convert-to-nested-vectors m) row v))
    (set-2d [m row column v]
      (mp/set-2d (mp/convert-to-nested-vectors m) row column v))
    (set-nd [m indexes v]
      (mp/set-nd (mp/convert-to-nested-vectors m) indexes v))
    (is-mutable? [m]
      false))

(extend-protocol mp/PBroadcast
  ISeq
    (broadcast [m new-shape]
      (mp/broadcast (mp/convert-to-nested-vectors m) new-shape)))

(extend-protocol mp/PBroadcastLike
  ISeq
    (broadcast-like [m a]
      (mp/broadcast (mp/convert-to-nested-vectors a) (mp/get-shape m))))

(extend-protocol mp/PSliceView
  ISeq
    (get-major-slice-view [m i]
      (nth m i)))

(extend-protocol mp/PSliceSeq
  ISeq
    (get-major-slice-seq [m] (vec m)))

(extend-protocol mp/PMatrixRows
  ISeq
    (get-rows [m]
      (vec m)))

(extend-protocol mp/PMatrixColumns
  ISeq
    (get-columns [m]
      (let [m (mp/coerce-param [] m)]
        (mp/get-columns m))))

(extend-protocol mp/PSliceSeq2
  ISeq
    (get-slice-seq [m dimension]
      (let [ldimension (long dimension)]
        (cond
         (== ldimension 0) (mp/get-major-slice-seq m)
         (< ldimension 0) (error "Can't get slices of a negative dimension: " dimension)
         :else (mapv #(mp/get-slice m dimension %) (range (mp/dimension-count m dimension)))))))

(extend-protocol mp/PConversion
  ISeq
    (convert-to-nested-vectors [m]
      (if (> (mp/dimensionality (first m)) 0)
        (mapv mp/convert-to-nested-vectors m)
        (vec m))))

(extend-protocol mp/PDimensionInfo
  ISeq
    (dimensionality [m]
      (inc (mp/dimensionality (first m))))
    (is-vector? [m]
      (== 0 (mp/dimensionality (first m))))
    (is-scalar? [m]
      false)
    (get-shape [m]
      #?(:cljs (js/console.log (str "shape of seq: " m)))
      (cons (count m) (mp/get-shape (first m))))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (mp/dimension-count (first m) (dec x)))))

(extend-protocol mp/PFunctionalOperations
  ISeq
    (element-seq [m]
      (if (== 0 (long (mp/dimensionality (first m))))
        m ;; handle 1D case, just return this sequence unchanged
        (mapcat mp/element-seq m)))
    (element-map
      ([m f]
        (mapv #(mp/element-map % f) m))
      ([m f a]
        (let [[m a] (mp/broadcast-compatible m a)]
          (mapv #(mp/element-map % f %2) m (mp/get-major-slice-seq a))))
      ([m f a more]
        (let [[m a & more] (apply mp/broadcast-compatible m a more)] ; FIXME
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

(extend-protocol mp/PMapIndexed
  ISeq
    (element-map-indexed
      ([ms f]
        (mapv (fn [i m] (mp/element-map-indexed m #(apply f (cons i %1) %&)))
              (range (count ms)) ms))
      ([ms f as]
        (let [[ms as] (mp/broadcast-compatible ms as)]
          (mapv (fn [i m a]
                  (mp/element-map-indexed m #(apply f (cons i %1) %&) a))
                (range (count ms)) ms (mp/get-major-slice-seq as))))
      ([ms f as more]
        (let [[ms as & more] (apply mp/broadcast-compatible ms as more)] ; FIXME
          (mapv (fn [i m a & mr]
                  (mp/element-map-indexed m #(apply f (cons i %1) %&) a mr))
                (range (count ms)) ms
                (mp/get-major-slice-seq as)
                (map mp/get-major-slice-seq more)))))
    (element-map-indexed!
      ([m f]
        (error "Sequence arrays are not mutable!"))
      ([m f a]
        (error "Sequence arrays are not mutable!"))
      ([m f a more]
        (error "Sequence arrays are not mutable!"))))

;; =====================================
;; Register implementation

;(imp/register-implementation '(1 2 3))
