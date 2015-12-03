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
  (:import [java.io Writer])
  (:import [clojure.lang IPersistentVector]
           [java.util List]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; a column-based DataSet implementation.
;; columns are arbitrary core.matrix arrays, treated as vectors

(deftype DataSetRow
  [^IPersistentVector column-names
   ^IPersistentVector columns
   ^long index]
  clojure.lang.Indexed
    (count [m] (count column-names))
    (nth [m i] 
      (mp/get-1d (nth columns i) index))
    (nth [m i not-found] 
      (when (< -1 i (long (mp/dimension-count (first columns) 0)))
        (mp/get-1d (nth columns i) index)))
  clojure.lang.Seqable
    (seq [m] 
      (seq (mp/convert-to-nested-vectors m)))
  clojure.lang.IPersistentCollection
    (cons [m v]
      (conj (mp/convert-to-nested-vectors m) v))
    (empty [m]
      [])
    (equiv [m a]
      (= (mp/convert-to-nested-vectors m) a))
  clojure.lang.IPersistentVector
    (length [m] (count column-names))
    (assocN [m i v]
      (assoc (mp/convert-to-nested-vectors m) i v)))

(defn- wrap-row [dataset index]
  (let [index (long index)
        cols (mp/get-columns dataset)]
    (when-not (< -1 index (long (mp/dimension-count (first cols) 0)))
      (error "Row index does not exist: " index))
    (DataSetRow. (mp/column-names dataset) cols index)))

(defrecord DataSet
  [^IPersistentVector column-names
   ^IPersistentVector columns
   ^IPersistentVector shape])

(defn dataset-from-columns [col-names cols]
  (let [^IPersistentVector col-names (vec col-names)
        cc (long (count col-names)) 
        ^IPersistentVector cols (vec (mp/get-rows (vec cols)))
        rc (long (mp/dimension-count (first cols) 0))]
    (when (not= cc (count cols))
      (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. col-names cols [rc cc])))

(defn dataset-from-rows [col-names rows]
  (let [^IPersistentVector col-names (vec col-names)
        cc (count col-names)
        rc (long (mp/dimension-count rows 0))
        ;; _ (println (str "Building dataset from rows with " cc " columns"))
        ^IPersistentVector cols (if (empty? rows)
                                  (into [] (repeat cc []))
                                  (into [] (mp/get-columns rows)))]
    (when (not= cc (count cols))
      (error "Mismatched number of columns, have: " cc " column names"))
    (DataSet. col-names cols [rc cc])))

(defn dataset-from-array
  "Construct a dataset from an array. Uses labels from the array if available."
  ([m]
    (let [dims (long (mp/dimensionality m))]
      (when (not= dims 2)
        (error "Can't construct dataset from array with shape: " (mp/get-shape m)))
      (let [col-names (or (mp/column-names m) (vec (range (mp/dimension-count m 1))))]
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

(extend-protocol mp/PConversion
  DataSetRow
    (convert-to-nested-vectors [m]
      (mapv #(nth m %) (range (count m)))))

(extend-protocol mp/PMatrixSlices
  DataSet
  (get-column [ds i]
    (nth (mp/get-columns ds) i))
  (get-row [ds i]
    (wrap-row ds i))
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
      (let [row-count (mp/dimension-count ds 0)]
        (mapv
          (fn [i] (wrap-row ds i))
          (range row-count)))))

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

(extend-protocol mp/PDimensionLabels
  DataSet
    (label [m dim i]
      (let [dim (long dim)]
        (cond 
          (== 1 dim) (nth (.column-names m) i)
          :else nil)))
    (labels [m dim]
      (let [dim (long dim)]
        (cond 
          (== 1 dim) (.column-names m)
          :else nil)))
    
  DataSetRow
    (label [m dim i]
      (let [dim (long dim)]
        (cond 
          (== 0 dim) (nth (.column-names m) i)
          :else nil)))
    (labels [m dim]
      (let [dim (long dim)]
        (cond 
          (== 0 dim) (.column-names m)
          :else nil)))) 

(extend-protocol mp/PColumnNames
  DataSet
    (column-name [m i]
      (nth (.column-names m) i))
    (column-names [m]
      (.column-names m))
    
  DataSetRow
    (column-name [m i]
      (nth (.column-names m) i))
    (column-names [m]
      (.column-names m))) 

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
	    (mp/new-matrix-nd [] shape))
	  (construct-matrix [m data]
	    (if (== 2 (long (mp/dimensionality data)))
	      (dataset-from-array data)
	      nil))
	  (supports-dimensionality? [m dims]
	    (<= 1 (long dims) 2))
   
   DataSetRow
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
	    (mp/new-matrix-nd [] shape))
	  (construct-matrix [m data]
	    (if (== 2 (long (mp/dimensionality data)))
	      (dataset-from-array data)
	      nil))
	  (supports-dimensionality? [m dims]
	    (<= 1 (long dims) 2)))

(extend-protocol mp/PDimensionInfo
  DataSet
    (dimensionality [m]
      2)
    (is-vector? [m]
      false)
    (is-scalar? [m] false)
    (get-shape [m]
      (.shape m))
    (dimension-count [m dim]
      (.nth ^IPersistentVector (.shape m) (long dim)))
    
  DataSetRow
    (dimensionality [m]
      1)
    (is-vector? [m]
      true)
    (is-scalar? [m] false)
    (get-shape [m]
      [(.length ^IPersistentVector (.column-names m))])
    (dimension-count [m dim]
      (let [dim (long dim)]
        (cond
          (== dim 0) (.length ^IPersistentVector (.column-names m))
          :else (error "Invalid dimension for dataset row: " dim)))))

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
          :else (error "Invalid dimensionality access with index: " (vec indexes)))))
    
  DataSetRow
    (get-1d [m x]
      (mp/get-1d (.nth ^IPersistentVector (.columns m) (long x)) (.index m)))
    (get-2d [m x y]
      (error "Invalid 2D access on DataSetRow"))
    (get-nd [m indexes]
      (let [dims (long (count indexes))]
        (cond
          (== 1 dims) (mp/get-1d (.nth ^IPersistentVector (.columns m) (long (first indexes))) (.index m))
          :else (error "Invalid ND access on DataSetRow")))))

(defn- broadcast-cols
  "Broadcasts a value to a vector of column values, in the shape specified"
  ^IPersistentVector [a [rows cols]]
  (case (long (mp/dimensionality a))
    0 (vec (repeat cols a))
    1 (mp/convert-to-nested-vectors a)
    2 (vec (mp/get-columns a))
    (error "Can't broadcast argument of shape " (mp/get-shape a))))

(extend-protocol mp/PFunctionalOperations
  DataSet
    (element-seq [m]
      (apply concat (mp/get-rows m)))
    (element-map
      ([m f]
        (reduce 
          (fn [m i]
            (update-in m [:columns i] mp/element-map f))
          m
          (range (.nth ^IPersistentVector (.shape m) 1))))
      ([m f a]
        (let [as (broadcast-cols a (mp/get-shape m))]
          (reduce 
            (fn [m i]
              (update-in m [:columns i] mp/element-map f (nth as i)))
            m
            (range (mp/dimension-count m 1)))))
      ([m f a more]
        (let [shape (.shape m)
              as (broadcast-cols a shape)
              mores (apply mapv vector (map #(broadcast-cols % shape) more))]
          (reduce 
            (fn [m i]
              (update-in m [:columns i] mp/element-map f (nth as i) (nth mores i)))
            m
            (range (second shape))))))
    (element-map!
      ([m f]
        (mp/assign! m (mp/element-map m f)))
      ([m f a]
        (mp/assign! m (mp/element-map m f a)))
      ([m f a more]
        (mp/assign! m (mp/element-map m f a more))))
    (element-reduce
      ([m f]
        (reduce f (mp/element-seq m)))
      ([m f init]
        (reduce f init (mp/element-seq m)))))

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

;; Printing methods for Datasets

(defmethod print-dup DataSet [^clojure.core.matrix.impl.dataset.DataSet x ^Writer writer] 
  (.write writer (str "#dataset/dataset " (into {} x))))

(defmethod print-method DataSet [^clojure.core.matrix.impl.dataset.DataSet x ^Writer writer] 
  (.write writer (str "#dataset/dataset " (into {} x))))

(defmethod print-dup DataSetRow [^clojure.core.matrix.impl.dataset.DataSetRow x ^Writer writer] 
  (.write writer (str "#dataset/row " {:column-names (.column-names x)
                                       :values (into [] x)})))

(defmethod print-method DataSetRow [^clojure.core.matrix.impl.dataset.DataSetRow x ^Writer writer] 
  (.write writer (str "#dataset/row " {:column-names (.column-names x)
                                       :values (into [] x)})))

;; reader methods for datasets

(defn read-dataset
  "Reader function for a core.matrix DataSet"
  ([a]
    (dataset-from-columns 
      (:column-names a) 
      (:columns a))))

(defn read-dataset-row
  "Reader function for a core.matrix DataSetRow. Creates a single row backed with persistent vectors"
  ([a]
    (DataSetRow. 
      (vec (:column-names a)) 
      (mapv vector (:values a)) 
      0)))

;; Dataset with a two named columns and both numeric and non-numeric data
(def CANONICAL-OBJECT (dataset-from-columns [:0 "Names"] [[1.0 2.0] ["A" "B"]]))

(imp/register-implementation CANONICAL-OBJECT)
