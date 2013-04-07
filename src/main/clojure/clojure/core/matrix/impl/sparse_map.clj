(ns clojure.core.matrix.impl.sparse-map
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp]))

;; =============================================================
;; core.matrix implementation enabling a map with appropriate
;; metadata to be used as a core.matrix implementation.
;;
;; map must be of { index-vector -> value }
;; where index-vector is a regular clojure vectors of longs
;;
;; metadata must include :shape
;;
;; metadata can include :default-value (default is nil)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn with-shape 
  ([m shape]
    (vary-meta m assoc :shape (to-long-array shape))))

(extend-protocol mp/PImplementation
  clojure.lang.IPersistentMap
    (implementation-key [m] :persistent-map)
    (new-vector [m length] (with-shape {} [length]))
    (new-matrix [m rows columns] (with-shape {} [rows columns]))
    (new-matrix-nd [m dims]
      (with-shape {} dims))
    (construct-matrix [m data]
      (let [sh (mp/get-shape data)]
        (with-shape
          (mp/element-reduce data 
                             (fn [mp v i] (if (nil? v) mp (assoc mp (vec i) v))) 
                             {} 
                             (base-index-seq-for-shape sh))
          sh)))
    (supports-dimensionality? [m dims]
      true))