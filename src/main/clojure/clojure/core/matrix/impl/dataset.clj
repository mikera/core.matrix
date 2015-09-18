(ns clojure.core.matrix.impl.dataset
  "Namespace for default core.matrix dataset implementation. 

   Represents a dataset as a 2D array such that:
   - Each column has an (optional) name as a label
   - Each column is stored as a separate array
   - Columns may have different types"
  (:require [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.impl default persistent-vector] ;; these are needed during loading
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :refer :all])
  (:import [clojure.lang IPersistentVector]
           [java.util List]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; a column-based DataSet implementation.
;; columns are arbitrary core.matrix arrays, treated as vectors

(defrecord DataSet
  [^IPersistentVector column-names
   ^IPersistentVector columns])

(defn dataset-from-columns [col-names cols]
  (let [^IPersistentVector col-names (vec col-names)
        cc (long (count col-names)) 
        ^IPersistentVector cols (vec (mp/get-rows (vec cols)))]
    (when (not= cc (count cols))
      (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. col-names cols)))

(defn dataset-from-rows [col-names rows]
  (let [^IPersistentVector col-names (vec col-names)
        cc (count col-names)
        ;; _ (println (str "Building dataset from rows with " cc " columns"))
        ^IPersistentVector cols (if (empty? rows)
                                  (into [] (repeat cc []))
                                  (into [] (mp/get-columns rows)))]
    (when (not= cc (count cols))
      (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. col-names cols)))

(defn dataset-from-array
  "Construct a dataset from an array. Uses labels from the array if available."
  ([m]
    (let [dims (long (mp/dimensionality m))]
      (when (not= dims 2)
        (error "Can't construct dataset from array with shape: " (mp/get-shape m)))
      (let [col-names (or (mp/labels m 1) (vec (range (mp/dimension-count m 1))))]
        (dataset-from-columns col-names (vec (mp/get-columns m)))))))

(defn dataset-from-row-maps
  ([col-names m]
     (let [rows (mapv (fn [row]
                        (reduce
                         (fn [acc c] (conj acc (get row c)))
                         [] col-names)) m)]
       (dataset-from-rows col-names rows))))

(extend-protocol mp/PDimensionLabels
  DataSet
    (label [m dim i]
      (let [dim (long dim)
            i (long i)]
        (cond
          (== dim 1) (nth (:column-names m) i)
          (<= 0 (long i) (dec (long (mp/dimension-count m dim)))) nil
          :else (error "Dimension index out of range: " i))))
    (labels [m dim]
      (let [dim (long dim)]
        (cond
          (== dim 1) (:column-names m)
          (<= 0 (long dim) (dec (long (mp/dimensionality m)))) nil
          :else (error "Dimension out of range: " dim)))))

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
      (->> (mp/get-columns ds)
           (apply mapv vector))))

(extend-protocol mp/PColumnSetting
  DataSet
    (set-column [ds i column]
      (let [scol (mp/get-column ds 0)
            column (mp/broadcast-like scol column)]
        (dataset-from-columns
          (.column-names ds)
          (assoc (.columns ds) i column)))))

(extend-protocol mp/PDatasetImplementation
  DataSet
    (column-names [ds]
      (.column-names ds))
    (select-columns [ds col-names]
      (let [^List all-col-names (.column-names ds)
            indices (mapv (fn [name] 
                            (let [ix (.indexOf all-col-names name)]
                              (when (== -1 ix) (error "Column name " name "not found in Dataset"))
                              ix)) 
                          col-names)
            cols (mapv #(.nth ^IPersistentVector (.columns ds) (long %)) indices)]
        (dataset-from-columns col-names cols)))
  (select-rows [ds rows]
    (let [col-names (mp/column-names ds)
          row-maps (mp/row-maps ds)]
      (try
        (->> (map #(nth row-maps %) rows)
             (dataset-from-row-maps col-names))
        (catch Exception e
          (let [c (long (count (mp/get-rows ds)))
                out-of-range (filter #(>= (long %) c) rows)]
            (if (> (long (count out-of-range)) 0)
              (error "Dataset contains only " c " rows. Can't select rows with indices: "
                     (vec out-of-range))
              (throw e)))))))
  (add-column [ds col-name col]
    (dataset-from-columns (conj (mp/column-names ds) col-name)
                          (conj (mp/get-columns ds) col)))
  (row-maps [ds]
    (let [col-names (mp/column-names ds)]
      (map #(zipmap col-names %)
           (mp/get-rows ds))))
  (to-map [ds]
    (into {} (map (fn [k v] [k v])
                  (mp/column-names ds)
                  (mp/get-columns ds))))
  (merge-datasets [ds1 ds2]
    (reduce
     (fn [acc [k v]]
       (let [^List colnames (mp/column-names acc)
             cols (mp/get-columns acc)
             idx (.indexOf colnames k)]
         (if (> idx -1)
           (dataset-from-columns colnames (assoc cols idx v))
           (dataset-from-columns (conj colnames k) (conj cols v)))))
     ds1 (mp/to-map ds2)))
  (rename-columns [ds col-map]
    (reduce
     (fn [acc [k v]]
       (let [^List colnames (mp/column-names acc)
             idx (.indexOf colnames k)]
         (if (> idx -1)
           (dataset-from-columns (assoc colnames idx v) (mp/get-columns acc))
           (error "Column " k " is not found in the dataset"))))
     ds col-map))
  (replace-column [ds col-name vs]
    (let [^List col-names (.column-names ds)
          idx (.indexOf col-names col-name)]
      (if (> idx -1)
        (mp/set-column ds idx vs)
        (error "Column " col-name " is not found in the dataset"))))
  (join-rows [ds1 ds2]
    (let [col-names-1 (mp/column-names ds1)
          col-names-2 (mp/column-names ds2)]
      (if (= (into #{} col-names-1)
             (into #{} col-names-2))
        (->> (mp/select-columns ds2 col-names-1)
             (mp/get-rows)
             (concat (mp/get-rows ds1))
             (dataset-from-rows col-names-1))
        (error "Can't join rows of datasets with different columns"))))
  (join-columns [ds1 ds2]
    (let [col-set-1 (into #{} (mp/column-names ds1))
          col-set-2 (into #{} (mp/column-names ds2))
          intersection (clojure.set/intersection col-set-1 col-set-2)]
      (if (zero? (count intersection))
        (dataset-from-columns
         (concat (mp/column-names ds1) (mp/column-names ds2))
         (concat (mp/get-columns ds1) (mp/get-columns ds2)))
        (error "Duplicate column names: " intersection)))))

(extend-protocol mp/PImplementation
  DataSet
  (implementation-key [m] :dataset)
  (meta-info [m]
    {:doc "clojure.core.matrix implementation for datasets"})
  (new-vector [m length]
    (mp/new-vector [] length))
  (new-matrix [m rows columns]
    (let [col-indexes (range columns)]
      (dataset-from-columns
       col-indexes
       (for [i col-indexes]
         (mp/new-vector (imp/get-canonical-object) rows)))))
  (new-matrix-nd [m shape]
    (if (== 2 (long (count shape)))
      (mp/new-matrix m (first shape) (second shape))
      nil))
  (construct-matrix [m data]
    (if (== 2 (long (mp/dimensionality data)))
      (dataset-from-array data)
      nil))
  (supports-dimensionality? [m dims]
    (== (long dims) 2)))

(extend-protocol mp/PDimensionInfo
  DataSet
    (dimensionality [m]
      2)
    (is-vector? [m]
      false)
    (is-scalar? [m] false)
    (get-shape [m]
      [(mp/dimension-count (first (.columns m)) 0) (.length ^IPersistentVector (.column-names m))])
    (dimension-count [m dim]
      (let [dim (long dim)]
        (cond
          (== dim 0) (mp/dimension-count (first (.columns m)) 0)
          (== dim 1) (.length ^IPersistentVector (.column-names m))
          :else (error "Invalid dimension for dataset: " dim)))))

(extend-protocol mp/PIndexedAccess
  DataSet
    (get-1d [m x]
      (mp/get-row m x))
    (get-2d [m x y]
      (mp/get-1d (.nth ^IPersistentVector (.columns m) (long y)) x))
    (get-nd [m indexes]
      (let [dims (long (count indexes))]
        (cond
          (== 1 dims) (mp/get-row m (first indexes))
          (== 2 dims) (mp/get-2d m (first indexes) (second indexes))
          (> 2 dims) (mp/get-nd (mp/get-2d m (first indexes) (second indexes)) (nnext indexes))
          :else (error "Invalid dimensionality access with index: " (vec indexes))))))

(extend-protocol mp/PIndexedSetting
  DataSet
    (set-1d [m x v]
      (error "Can't do 1D set on a DataSet"))
    (set-2d [m x y v]
      (let [col (mp/get-column m y)]
        (dataset-from-columns
          (mp/column-names m)
          (assoc (mp/get-columns m) y (assoc col x v)))))
    (set-nd [m indexes v]
      (let [dims (long (count indexes))]
        (cond
          (== 2 dims) (mp/set-2d m (first indexes) (second indexes) v)
          :else (error "Can't set on DataSet array with index: " (vec indexes)))))
    (is-mutable? [m] false))

;; Dataset with a two named columns and both numeric and non-numeric data
(def CANONICAL-OBJECT (dataset-from-columns [:0 "Names"] [[1.0 2.0] ["A" "B"]]))

(imp/register-implementation CANONICAL-OBJECT)
