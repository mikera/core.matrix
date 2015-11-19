(ns clojure.core.matrix.readers
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.dataset :as ds] 
            [clojure.core.matrix.impl.dataset :as dimpl]))

(defn dataset
  "Reader function for a core.matrix DataSet"
  ([a]
    (dimpl/dataset-from-columns 
      (:column-names a) 
      (:columns a))))

(defn dataset-row
  "Reader function for a core.matrix DataSetRow. Creates a single row backed with persistent vectors"
  ([a]
    (clojure.core.matrix.impl.dataset.DataSetRow. 
      (vec (:column-names a)) 
      (mapv vector (:values a)) 
      0)))


