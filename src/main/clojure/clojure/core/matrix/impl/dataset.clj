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

(defn construct-dataset [col-names columns]
  (let [^IPersistentVector col-names (vec col-names)
        ^IPersistentVector columns (vec columns)
        cc (count col-names)]
    (when (not= cc (count columns))
      (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. col-names columns)))

(defn dataset-from-array
  ([m]
     (when (mp/is-scalar? m)
       (error "Don't know how to construct DataSet from type: " (class m)))
     (when (< (mp/dimensionality m) 2)
       (error "Can't construct dataset from array with shape: " (mp/get-shape m)))
     (let [row-count (mp/dimension-count m 0)
           col-count (mp/dimension-count m 1)
           col-indexes (range col-count)]
       (construct-dataset
        (into [] col-indexes)
        (vec (for [i col-indexes]
               (mp/get-slice m 1 i)))))))

(extend-protocol mp/PMatrixSlices
  DataSet
  (get-column [ds i]
    (nth (mp/get-columns ds) i))
  (get-row [ds i]
    (wrap/wrap-slice ds i))
  (get-major-slice [ds i]
    (mp/get-major-slice (mp/get-rows ds) i))
  (get-slice [ds dimension i]
    (mp/get-slice (mp/get-rows ds) dimension i)))

(extend-protocol mp/PMatrixColumns
  DataSet
  (get-columns [ds]
    (.columns ds)))

(extend-protocol mp/PMatrixRows
  DataSet
  (get-rows [ds]
    (mp/transpose (mp/get-columns ds))))

(extend-protocol mp/PColumnSetting
  DataSet
  (set-column [ds i column]
    (let [scol (mp/get-column ds 0)
          column (mp/broadcast-like scol column)]
      (construct-dataset
       (mp/column-names ds)
       (assoc (mp/get-columns ds) i column)))))

(extend-protocol mp/PDatasetImplementation
  DataSet
  (column-names [ds]
    (.column-names ds))
  (select-columns [ds col-names]
    (let [all-col-names (mp/column-names ds)
          indices (map #(.indexOf all-col-names %) col-names)
          cols (map #(mp/get-column ds %) indices)]
      (construct-dataset col-names cols)))
  (add-column [ds col-name col]
    (construct-dataset (conj (mp/column-names ds) col-name)
             (conj (mp/get-columns ds) col)))
  (to-map [ds]
    (into {} (map (fn [k v] [k v])
                  (mp/column-names ds)
                  (mp/get-columns ds))))
  (merge-datasets [ds1 ds2]
    (reduce
     (fn [acc [k v]]
       (let [colnames (mp/column-names acc)
             cols (mp/get-columns acc)
             idx (.indexOf colnames k)]
         (if (> idx -1)
           (construct-dataset colnames (assoc cols idx v))
           (construct-dataset (conj colnames k) (conj cols v)))))
     ds1 (mp/to-map ds2)))
  (rename-columns [ds col-map]
    (reduce
     (fn [acc [k v]]
       (let [colnames (mp/column-names acc)
             idx (.indexOf colnames k)]
         (if (> idx -1)
           (construct-dataset (assoc colnames idx v) (mp/get-columns acc))
           (error "Column " k " is not found in the dataset"))))
     ds col-map))
  (replace-column [ds col-name vs]
    (let [col-names (.column-names ds)
          idx (.indexOf col-names col-name)]
      (if (> idx -1)
        (mp/set-column ds idx vs)
        (error "Column " col-name " is not found in the dataset"))))
  (conj-rows [ds1 ds2]
    (let [col-names-1 (mp/column-names ds1)
          col-names-2  (mp/column-names ds2)]
      (if (= (into #{} col-names-1)
             (into #{} col-names-2))
        (construct-dataset
         col-names-1
         (mapv #(into %1 %2)
               (mp/get-columns ds1)
               (-> (mp/select-columns ds2 col-names-1)
                   (mp/get-columns))))
        (error "Can't join rows of datasets with different columns")))))


(extend-protocol mp/PImplementation
  DataSet
  (implementation-key [m] :dataset)
  (meta-info [m]
    {:doc "clojure.core.matrix implementation for datasets"})
  (new-vector [m length]
    (mp/new-vector [] length))
  (new-matrix [m rows columns]
    (let [col-indexes (range columns)]
      (construct-dataset
       col-indexes
       (for [i col-indexes]
         (mp/new-vector (imp/get-canonical-object) rows)))))
  (new-matrix-nd [m shape]
    (if (== 2 (count shape))
      (mp/new-matrix m (first shape) (second shape))
      nil))
  (construct-matrix [m data]
    (if (== 2 (mp/dimensionality data))
      (dataset-from-array data)
      nil))
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
    [(mp/dimension-count m 0) (mp/dimension-count m 1)])
  (dimension-count [m x]
    (cond
     (== x 0) (mp/dimension-count (first (mp/get-columns m)) 0)
     (== x 1) (count (mp/column-names m))
     :else (error "Invalid dimension: " x))))

(extend-protocol mp/PIndexedAccess
  DataSet
    (get-1d [m x]
      (mp/get-row m x))
    (get-2d [m x y]
      (mp/get-major-slice (mp/get-column m y) x))
    (get-nd [m indexes]
      (let [dims (long (count indexes))]
        (cond
         (== 1 dims) (mp/get-row m (first indexes))
         (== 2 dims) (mp/get-2d m (first indexes) (second indexes))
         (> 2 dims) (mp/get-nd (mp/get-2d m (first indexes) (second indexes)) (nnext indexes))
         :else
         (error "Invalid dimensionality access with index: " (vec indexes))))))

(extend-protocol mp/PIndexedSetting
  DataSet
    (set-1d [m x v]
      (error "Can't do 1D set on a DataSet"))
    (set-2d [m x y v]
      (let [col (mp/get-column m y)]
        (construct-dataset
         (mp/column-names m)
         (assoc (mp/get-columns m) y (assoc col x v)))))
    (set-nd [m indexes v]
      (let [dims (long (count indexes))]
        (cond
          (== 2 dims) (mp/set-2d m (first indexes) (second indexes) v)
          :else (error "Can't set on DataSet array with index: " (vec indexes)))))
    (is-mutable? [m] false))

(def CANONICAL-OBJECT (construct-dataset [:0] [[1.0 2.0]]))

(imp/register-implementation CANONICAL-OBJECT)
