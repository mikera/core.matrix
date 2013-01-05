(ns core.matrix.impl.persistent-vector
  (:use core.matrix)
  (:refer-clojure :exclude [vector?]))

;; =======================================================================
;; Implementation for standard Clojure persistent vectors used as matrices

(extend-protocol PIndexedAccess
  clojure.lang.IPersistentVector
    (get-1d [m x]
      (.nth m (int x)))
    (get-2d [m x y]
      (let [row (.nth m (int x))]
        (get-1d row y)))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.nth m (int (first indexes)))]
          (get-nd m next-indexes))
        (.nth m (int (first indexes))))))

(defn coerce-nested 
  "Ensures a vector is fully coerced"
  ([v]
    (mapv #(if (number? %) % (coerce-nested %)) v)))

(extend-protocol PCoercion
  clojure.lang.IPersistentVector
    (coerce-param [m param]
      (cond
        (vector? param) param
        (sequential? param) (coerce-nested param)
        :default (error "Can't coerce to vector: " (class param)))))

(extend-protocol PMatrixMultiply
  clojure.lang.IPersistentVector
    (mmultiply [m a]
      (let [a (coerce-param m a)]
        (error "Not yet implemented"))))

(extend-protocol PMatrixDimensionInfo
  clojure.lang.IPersistentVector
    (dimensionality [m]
      (let [fst (.get m 0)]
        (if (number? fst) 
          1
          (inc (dimensionality fst)))))
    (row-count [m]
      (count m))
    (column-count [m]
      (count (m 0)))
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (dimension-count (m 0) (dec x)))))

