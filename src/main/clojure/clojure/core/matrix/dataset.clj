(ns clojure.core.matrix.dataset
  "Namespace for the core.matrix dataset API. Datasets are similar to 2D matrices, except that they support labelled
   columns and operations on labelled columns."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.dataset :as impl]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [error]])
  (:import [clojure.core.matrix.impl.dataset DataSet]
           [java.util List]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ===============================================================
;; Specialised functions for dataset handling

(defmacro dataset?
  "Returns true if argument is a dataset."
  ([d]
    `(instance? DataSet ~d)))

(defn dataset
  "Creates dataset from on of the following:
   1. matrix - its columns will be used as dataset columns.
   2. seq of rows
   3. seq of row maps (column names -> values for each row)
   4. map of columns with associated list of values.
   5. seq of maps

   If col-names are provided they will be used, else incrementing Long values starting from 0, 
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
      (dataset? data) data
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
            (error "Can't create dataset from incomplete maps")))
        
       :else 
         (error "Don't know how to create dataset from data of type " (class data)))))

(defn column-names
  "Returns a persistent vector containing column names in the same order as they are placed in the dataset."
  ([ds]
    (mp/column-names ds)))

(defn column-name
  "Returns column name at given index. Returns nil if index is not found."
  ([ds idx]
    (nth (mp/column-names ds) idx)))

(defn dimension-name
  "Returns the name for a given index along the specified dimension."
  ([ds dim idx]
    (mp/dimension-name ds dim idx)))

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

(defn except-columns
  "Returns new dataset with all columns except specified."
  ([ds col-names]
    (let [col-names (clojure.set/difference
                      (into #{} (column-names ds))
                      (into #{} col-names))]
      (mp/select-columns ds col-names))))

(defn row-maps
  "Returns vector of maps with row values."
  ([ds]
    (mp/row-maps ds)))

(defn to-map
  "Returns map of columns with associated list of values."
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

(defn update-column
  "Applies function f to column in a dataset"
  ([ds col-name f & args]
     (let [^List col-names (mp/column-names ds)
           col (mp/get-column ds (.indexOf col-names col-name))]
       (mp/replace-column
         ds col-name
         (matrix col (map #(apply f % args) col))))))

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
