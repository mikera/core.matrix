(ns clojure.core.matrix.impl.sparse-map
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp]))

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
    (meta-info [m]
      {:doc "Core.matrix implementation enabling a map with appropriate
             metadata to be used as a core.matrix implementation."})
    (new-vector [m length] 
      (vary-meta (with-shape {} [length]) assoc :default-value 0.0))
    (new-matrix [m rows columns] 
      (vary-meta (with-shape {} [rows columns]) assoc :default-value 0.0))
    (new-matrix-nd [m dims]
      (with-shape {} dims))
    (construct-matrix [m data]
      (let [sh (mp/validate-shape data)]
        (with-shape
          (reduce
            (fn [mp [v i]] (if (nil? v) mp (assoc mp (vec i) v)))
            {}
            (map vector (mp/element-seq data) (base-index-seq-for-shape sh)))
          sh)))
    (supports-dimensionality? [m dims]
      true))

(extend-protocol mp/PDimensionInfo
  clojure.lang.IPersistentMap
    (dimensionality [m]
      (if-let [sh (:shape (meta m))]
        (count sh)
        0))
    (is-vector? [m]
      (if-let [sh (:shape (meta m))]
        (== 1 (count sh))
        false))
    (is-scalar? [m]
      (nil? (:shape (meta m))))
    (get-shape [m]
      (:shape (meta m)))
    (dimension-count [m x]
      (nth (:shape (meta m)) x)))

(extend-protocol mp/PIndexedAccess
  clojure.lang.IPersistentMap
    (get-1d [m x]
      (or (m [x]) (:default-value (meta m))))
    (get-2d [m x y]
      (or (m [x y]) (:default-value (meta m))))
    (get-nd [m indexes]
      (or (m (vec indexes)) (:default-value (meta m)))))

(extend-protocol mp/PIndexedSetting
  clojure.lang.IPersistentMap
    (set-1d [m row v]
      (assoc m [row] v))
    (set-2d [m row column v]
      (assoc m [row column] v))
    (set-nd [m indexes v]
      (assoc m (vec indexes) v))
    (is-mutable? [m]
      false))

(imp/register-implementation (with-shape {} [1 1]))