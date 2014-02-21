(ns clojure.core.matrix.impl.dataset
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [clojure.lang IPersistentVector]))

;; TODO
;; a column based DataSet implementation.
;; columns are arbitrary core.matrix arrays, treated as vectors

(defrecord DataSet
  [^IPersistentVector column-names
   ^IPersistentVector columns])

(defmacro row-count [^DataSet d]
  `(mp/dimension-count (first (.columns ~d)) 0))

(defmacro column-count [^DataSet d]
  `(count (.column-names ~d)))

(defmacro row [^DataSet d i]
  `(wrap/wrap-slice ~d ~i))

(defmacro column [^DataSet d i]
  `(.nth (.columns ~d) (int ~i)))

(defmacro element [^DataSet d i j]
  ;; note we use get-slice here, because column may have more than one dimension
  `(mp/get-major-slice (column ~d ~j) ~i))

(defn- dataset [^IPersistentVector column-names ^IPersistentVector columns]
  (let [cc (count column-names)]
    (when (not= cc (count columns)) (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. column-names columns)))

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

(extend-protocol mp/PIndexedAccess
  DataSet
    (get-1d [m x]
      (row m x))
    (get-2d [m x y]
      (element m x y))
    (get-nd [m indexes]
      (let [dims (long (count indexes))]
        (cond
          (== 1 dims) (row m (first indexes))
          (== 2 dims) (element m (first indexes) (second indexes)) 
          (< 2 dims) (mp/get-nd (element m (first indexes) (second indexes)) (nnext indexes)) 
          :else
            (error "Invalid dimensionality access with index: " (vec indexes))))))

(imp/register-implementation (dataset [:0] [["Foo"]]))