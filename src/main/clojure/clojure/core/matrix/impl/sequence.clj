(ns clojure.core.matrix.impl.sequence
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.multimethods :as mm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(extend-protocol mp/PImplementation
  clojure.lang.ISeq
    (implementation-key [m] :sequence)
    (new-vector [m length] (vec (repeat length 0.0)))
    (new-matrix [m rows columns] (vec (repeat rows (mp/new-vector m columns))))
    (new-matrix-nd [m dims]
      (if-let [dims (seq dims)]
        (vec (repeat (first dims) (mp/new-matrix-nd m (next dims))))
        0.0))
    (construct-matrix [m data]
      (let [dims (mp/dimensionality data)]
        (cond
	        (== dims 0) (if (mp/is-scalar? data) data (mp/get-0d data))
	        (>= dims 1)
	          (mapv #(mp/construct-matrix m %) (for [i (range (mp/dimension-count data 0))] (mp/get-major-slice data i)))
	        (sequential? data)
	          (mapv #(mp/construct-matrix m %) data)
	        :default
	          (error "Don't know how to construct matrix from: " (class data)))))
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

(extend-protocol mp/PIndexedSetting
  java.lang.Object
    (set-1d [m row v]
      (mp/set-1d (mp/convert-to-nested-vectors m) row v))
    (set-2d [m row column v]
      (mp/set-2d (mp/convert-to-nested-vectors m) row column v))
    (set-nd [m indexes v]
      (mp/set-nd (mp/convert-to-nested-vectors m) indexes v))
    (is-mutable? [m]
      false))

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
      (mapv mp/convert-to-nested-vectors m)))

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
        (mapv #(mp/element-map % f %2) m a))
      ([m f a more]
        (mapv #(mp/element-map % f %2 %3) m a more)))
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
        (reduce f (mapcat mp/element-seq m)))
      ([m f init]
        (reduce f init (mapcat mp/element-seq m)))))

;; =====================================
;; Register implementation

(imp/register-implementation '())
