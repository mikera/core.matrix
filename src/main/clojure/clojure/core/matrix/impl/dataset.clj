(ns clojure.core.matrix.impl.dataset
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [clojure.lang IPersistentVector]))

;; TODO
;; a column based DataSet implementation.
;; columns are arbitrary core.matrix arrays, treated as vectors

(defrecord DataSet
  [^IPersistentVector column-names
   ^IPersistentVector columns])

(defn- dataset [^IPersistentVector column-names ^IPersistentVector columns]
  (DataSet. column-names columns))

(defn- dataset-from-array 
  ([m]
    (when (not (mp/is-scalar? m)) (error "Don't know how to construct DataSet from type: " (class m)))
    (when (< (mp/dimensionality m) 2) (error "Can't construct dataset from array with shape: " (mp/get-shape m)))
    (let [row-count (mp/dimension-count m 0)
          col-count (mp/dimension-count m 1)
          col-indexes (range col-count)]
      (dataset (mapv keyword col-indexes)
                (vec (for [i col-indexes]
                      (mp/get-slice m 1 i)))))))

(defmacro row-count [d]
  `(mp/dimension-count (first (.columns ~d)) 0))

(defmacro column-count [d]
  `(count (.column-names ~d)))

(extend-protocol mp/PImplementation
  DataSet
    (implementation-key [m] :dataset)
    (meta-info [m]
      {:doc "Clojure.core.matrix implementation for datasets"})
    (new-vector [m length] (error "Can't construct a Dataset as a 1D vector, only 2D supported"))
    (new-matrix [m rows columns] 
      (let [col-indexes (range columns)] 
        (DataSet. (mapv keyword col-indexes) (for [i (col-indexes)] (mp/new-vector @#'clojure.core.matrix/*matrix-implementation* rows)))))
    (new-matrix-nd [m shape]
      (if (== 2 (count shape))
        (mp/new-matrix m (first shape) (second shape))
        (error "Can't construct a new DataSet with shape: " shape)))
    (construct-matrix [m data]
      (dataset-from-array data))
    (supports-dimensionality? [m dims]
      (== dims 2)))


(extend-protocol mp/PDimensionInfo
  DataSet
    (dimensionality [m] 
      2)
    (is-vector? [m] 
      false)
    (is-scalar? [m] false)
    (get-shape [m] 
      [(row-count m) (column-count m)])
    (dimension-count [m x]
      (cond 
          (== x 0) (row-count m) 
          (== x 1) (column-count m)
          :else (error "Invalid dimension: " x))))

(imp/register-implementation (DataSet. [:0] [["Foo"]]))