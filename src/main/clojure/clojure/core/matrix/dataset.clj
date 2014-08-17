(ns clojure.core.matrix.dataset
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.impl.dataset)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:import clojure.core.matrix.impl.dataset.DataSet))

(defmacro dataset?
  "Returns true if argument is a dataset"
  ([d]
     `(instance? DataSet ~d)))

(defn dataset
  "Creates dataset from:
    column names and seq of rows
    column names and seq of row maps
    map of columns with associated list of values.
    matrix - its columns will be used as dataset columns and incrementing Long values starting from 0, i.e. 0, 1, 2, etc will be used as column names.
    seq of maps"
  ([col-names m]
     (cond
      (matrix? m) (dataset-from-rows col-names m)
      (and (vec? m) (empty? m)) (dataset-from-rows col-names m)
      (map? (first m)) (dataset-from-row-maps col-names m)
      (map? m) (let [cols (reduce
                           (fn [acc c] (conj acc (get m c)))
                           [] col-names)]
                 (dataset-from-columns col-names cols))
      :else (error "Don't know how to create dataset from shape"  (shape m))))
  ([m]
     (cond
      (dataset? m) m
      (matrix? m) (dataset-from-array m)
      (map? m)
      (let [col-names (keys m)
            cols (reduce
                  (fn [acc c] (conj acc (get m c)))
                  [] col-names)
            row-counts (into #{} (map count m))]
        (if (= (count row-counts) 1)
          (dataset-from-columns col-names cols)
          (error "Cant' create dataset with different column lengths")))

      (and (= (mp/dimensionality m) 1)
           (map? (first m)))
      (let [col-names (keys (first m))
            col-map (-> (zipmap col-names (repeat []))
                        (vector)
                        (#(apply conj % m))
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
          (dataset-from-columns
           col-names
           (reduce #(conj %1 (get col-map %2)) [] col-names))
          (error "Can't create dataset from incomplete maps"))))))

(defn column-names
  "Returns a persistent vector containing column names in the same order as they are placed in the dataset"
  ([ds]
     (mp/column-names ds)))

(defn column-name
  "Returns column name at given index. Returns nil if index is not found"
  ([ds idx]
     (get (mp/column-names ds) idx)))

(defn dimension-name
  "Returns the name for a given index along the specified dimension"
  ([ds dim idx]
    (mp/dimension-name ds dim idx)))

(defn add-column
  "Adds column to the dataset"
  ([ds col-name col]
     (mp/add-column ds col-name col)))

(defn select-columns
  "Produces a new dataset with the columns in the specified order"
  ([ds col-names]
     (mp/select-columns ds col-names)))

(defn select-rows
  "Produces a new dataset with the rows in the specified order"
  ([ds rows]
     (mp/select-rows ds rows)))

(defn except-columns
  "Returns new dataset with all columns except specified"
  ([ds col-names]
     (let [col-names (clojure.set/difference
                      (into #{} (column-names ds))
                      (into #{} col-names))]
       (mp/select-columns ds col-names))))

(defn row-maps
  "Returns vector of maps with row values"
  ([ds]
     (mp/row-maps ds)))

(defn to-map
  "Returns map of columns with associated list of values"
  ([ds]
     (mp/to-map ds)))

(defn merge-datasets
  "Returns a dataset created by combining columns of the given datasets. In case of columns with duplicate names, last-one-wins strategy is applied."
  ([ds1 ds2] (mp/merge-datasets ds1 ds2))
  ([ds1 ds2 & args]
     (apply merge-datasets (merge-datasets ds1 ds2) args)))

(defn rename-columns
  "Renames columns based on map of old new column name pairs"
  ([ds col-map]
     (mp/rename-columns ds col-map)))

(defn replace-column
  "Replaces column in a dataset with new values"
  ([ds col-name vs]
     (mp/replace-column ds col-name vs)))

(defn update-column
  "Applies function f to column in a dataset"
  ([ds col-name f & args]
     (let [col-names (mp/column-names ds)
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
