(ns clojure.core.matrix.dataset
  "Namespace for the core.matrix dataset API. 

   Datasets can be considered as 2D matrices, and in addition they support labelled columns and 
   a set of specialised operations on labelled columns.

   Dataset rows are also labelled by columns.
   "
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.dataset :as impl]
            [clojure.core.matrix :as cm :refer [matrix? vec? shape
                                                get-column emap]]
            [clojure.core.matrix.macros :refer [error]]
            [clojure.core.matrix.utils :as utils])

  #?(:clj (:import [clojure.core.matrix.impl.dataset DataSet]
                   [clojure.lang IPersistentVector]
                   [java.util List])
     :cljs (:require-macros [clojure.core.matrix.macros :refer [error]]
                            [clojure.core.matrix.macros-clj :refer [error?]])))

#?(:clj (do (set! *warn-on-reflection* true)
            (set! *unchecked-math* :warn-on-boxed)))

;; ===============================================================
;; Specialised functions for dataset handling
(defn dataset?
  "Returns true if argument is a dataset, defined as a 1D or 2D array with column names"
  (^Boolean [d]
   ( #?(:clj boolean :cljs and) (and (<= 1 (long (mp/dimensionality d)) 2) 
                                     (mp/column-names d)))))

(defn dataset-row?
  "Returns true if argument is a datasetrow type, defined as a 1D array with column names"
  (^Boolean [d]
   ( #?(:clj boolean :cljs and) (and (= 1 (long (mp/dimensionality d))) 
                                     (mp/column-names d)))))

(defn- dataset-from-map-seq
  "Returns a dataset created from a sequence of maps"
  [data]
 (let [col-names (keys (first data))
              col-map (-> (zipmap col-names (repeat []))
                          (vector)
                          (#(apply conj % data))
                          (#(apply merge-with conj %)))]
          ;; check that there all the maps have the same keys
          ;; no additional keys in all but the first maps and
          ;; lengths of rows are equal
          (if (and (= (into #{} col-names)
                      (into #{} (keys col-map)))
                   (->> (vals col-map)
                        (map count)
                        (into #{})
                        (count)
                        (= 1)))
            (impl/dataset-from-columns
              col-names
              (reduce #(conj %1 (get col-map %2)) [] col-names))
            (error "Can't create dataset from incomplete maps"))))

(defn dataset
  "Creates dataset from one of the following:
    1. matrix - its columns will be used as dataset columns.
    2. seq of rows
    3. seq of row maps (column names -> values for each row)
    4. map of columns with associated list of values.
    5. existing dataset

   If column names are provided then they will be used, else incrementing Long values starting from 0,
   i.e. 0, 1, 2, etc will be used as column names"
  ([col-names data]
    (cond
      (matrix? data) (impl/dataset-from-rows col-names data)
      (and (vec? data) (empty? data)) (impl/dataset-from-rows col-names data)
      (map? (first data)) (impl/dataset-from-row-maps col-names data)
      (map? data) (let [cols (reduce
                           (fn [acc c] (conj acc (get data c)))
                           [] col-names)]
                    (impl/dataset-from-columns col-names cols))
      :else (error "Don't know how to create dataset from shape"  (shape data))))

  ([data]
    (cond
      ;; already a dataset, just return as-is
      (dataset? data) data
      
      (dataset-row? (first data))
      (->> data
             (mapv #(zipmap (mp/column-names %) (mp/get-columns %)))
             dataset-from-map-seq)

      ;; construct a dataset using 2D matrix data
      (matrix? data) (impl/dataset-from-array data)

      ;; map of names -> columns of elements
      (map? data)
        (let [col-names (keys data)
              cols (reduce
                    (fn [acc c] (conj acc (get data c)))
                    [] col-names)
              row-counts (into #{} (map count data))]
          (if (= (count row-counts) 1)
            (impl/dataset-from-columns col-names cols)
            (error "Can't create dataset with different column lengths")))

      ;; 1D sequence of maps of column name -> value
      (and (= (mp/dimensionality data) 1)
           (map? (first data)))
        (dataset-from-map-seq data) 
      
       :else
       (error "Don't know how to create dataset from data of type " ( #?(:clj class
                                                                         :cljs type)
                                                                     data)))))

(defn column-names
  "Gets the column names from a DataSet, DatsetRiow or labelled array

   Returns a persistent vector containing column names in the same order as they are placed in the dataset.
   Returns nil if no coklumn names are defined (i.e. the array has no column labels) "
  ([ds]
    (mp/column-names ds)))

(defn column-name
  "Returns column name at given index.

   Throws an exception if the index is out of bounds."
  ([ds column-index]
    (mp/column-name ds column-index)))

(defn column-index
  "Returns column index for a given column name.

   Returns nil if the column name does not exist."
  ([ds column-name]
    (mp/column-index ds column-name)))

(defn column
  "Gets a named column from the dataset. Throws an error if the column does not exist.

   Works on labelled arrays, datsets or dataset rows."
  ([ds col-name]
   (if-let [ix (if (number? col-name) col-name (mp/column-index ds col-name))]
     (get-column ds ix)
     (error "Column name not found: " col-name))))

(defn add-column
  "Adds column to the dataset."
  ([ds col-name col]
    (mp/add-column ds col-name col)))

(defn select-columns
  "Produces a new dataset with the columns in the specified order."
  ([ds col-names]
    (mp/select-columns ds col-names)))

(defn select-rows
  "Produces a new dataset with the rows in the specified order."
  ([ds rows]
    (mp/select-rows ds rows)))

(defn remove-columns
  "Returns new dataset with the specified columns removed"
  ([ds col-names]
    (let [col-names (into #{} col-names)
          selected (filterv (complement col-names) (column-names ds))]
      (mp/select-columns ds selected))))

(defn ^{:deprecated true} except-columns
  "Returns new dataset with the specified columns removed.

   DEPRECATED: please use remove-columns instead"
  ([ds col-names]
    (let [col-names (into #{} col-names)
          selected (filterv (complement col-names) (column-names ds))]
      (mp/select-columns ds selected))))

(defn row-maps
  "Returns vector of maps with row values."
  ([ds]
    (mp/row-maps ds)))

(defn to-map
  "Returns map of column name -> columns (i.e. vectors of values in each column)."
  ([ds]
    (mp/to-map ds)))

(defn merge-datasets
  "Returns a dataset created by combining columns of the given datasets. In case of columns with duplicate names, last-one-wins strategy is applied."
  ([ds1 ds2]
    (mp/merge-datasets ds1 ds2))
  ([ds1 ds2 & args]
    (apply merge-datasets (merge-datasets ds1 ds2) args)))

(defn rename-columns
  "Renames columns based on map of old column name -> new column name pairs"
  ([ds col-map]
     (mp/rename-columns ds col-map)))

(defn replace-column
  "Replaces column in a dataset with new values"
  ([ds col-name vs]
   (mp/replace-column ds col-name vs)))

(defn emap-column
  "Applies function f to every element in a column in a dataset.

   Extra args to the function may be supplied."
  ([ds col-name f & args]
   (let [^List col-names (mp/column-names ds)
         col (mp/get-column ds (.indexOf col-names col-name))]
     (mp/replace-column
      ds col-name
      (apply emap f col args)))))

(defn emap-columns
  "Applies a function to the specified set of columns. Calls emap-column for each column specified.

   Optionally a map of {column name -> function} may be provided."
  ([ds col-names f]
    (reduce (fn [ds col] (emap-column ds col f))
            ds col-names))
  ([ds col-names f & args]
    (reduce (fn [ds col] (apply emap-column ds col f args))
            ds col-names))
  ([ds col-fn-map]
    (reduce (fn [ds [col f]] (emap-column ds col f))
            ds col-fn-map)))

(defn join-rows
  "Returns a dataset created by combining the rows of the given datasets"
  ([ds1 ds2]
    (mp/join-rows ds1 ds2))
  ([ds1 ds2 & args]
    (apply mp/join-rows (mp/join-rows ds1 ds2) args)))

(defn join-columns
  "Returns a dataset created by combining the columns of the given datasets"
  ([ds1 ds2]
    (mp/join-columns ds1 ds2))
  ([ds1 ds2 & args]
    (apply mp/join-columns (mp/join-columns ds1 ds2) args)))
