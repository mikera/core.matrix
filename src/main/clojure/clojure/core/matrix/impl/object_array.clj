(ns clojure.core.matrix.impl.object-array
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; clojure.core.matrix implementation for Java double arrays
;;
;; Useful as a fast, mutable implementation.

(def OBJECT-ARRAY-CLASS (Class/forName "[Ljava.lang.Object;"))

(defn construct-object-array ^objects [data]
  (let [dims (long (mp/dimensionality data))]
    (cond
      (== dims 1)
        (let [n (long (mp/dimension-count data 0))
              r (object-array n)]
           (dotimes [i n]
             (aset r i (mp/get-1d data i)))
           r)
      (== dims 0)
        (mp/get-0d data)
      :default
        (object-array (map construct-object-array (mp/get-major-slice-seq data))))))

(defn construct-nd ^objects [shape]
  (let [dims (long (count shape))] 
        (cond 
          (== 1 dims) (object-array (long (first shape)))
          (> dims 1)  
            (let [n (long (first shape))
                  m (object-array n)
                  ns (next shape)]
              (dotimes [i n]
                (aset m i (construct-nd ns)))
              m)
          :else (error "Can't make a nested object array of dimensionality: " dims))))

(extend-protocol mp/PImplementation
  (Class/forName "[Ljava.lang.Object;")
    (implementation-key [m] :object-array)
    (meta-info [m]
      {:doc "Clojure.core.matrix implementation for Java double arrays"})
    (new-vector [m length] (object-array (int length)))
    (new-matrix [m rows columns] 
      (let [columns (int columns)
            m (object-array rows)]
        (dotimes [i rows]
          (aset m i (object-array columns)))
        m))
    (new-matrix-nd [m shape]
      (construct-nd shape))
    (construct-matrix [m data]
      (construct-object-array data))
    (supports-dimensionality? [m dims]
      (>= dims 1)))


(extend-protocol mp/PDimensionInfo
  (Class/forName "[Ljava.lang.Object;")
    (dimensionality [m] 
      (let [^objects m m] 
        (+ 1 (mp/dimensionality (aget m 0)))))
    (is-vector? [m] 
      (let [^objects m m]
        (or 
         (== 0 (alength m))
         (== 0 (mp/dimensionality (aget m 0))))))
    (is-scalar? [m] false)
    (get-shape [m] 
      (let [^objects m m]
        (if (== 0 (alength m))
           1
           (cons (alength m) (mp/get-shape (aget m 0))))))
    (dimension-count [m x]
      (let [^objects m m
            x (long x)] 
        (cond 
          (== x 0)
            (alength m)
          (> x 0)
            (mp/dimension-count (aget m 0) (dec x))
          :else
            (error "Invalid dimension: " x)))))

;; explicitly specify we use a primitive type
(extend-protocol mp/PTypeInfo
  (Class/forName "[Ljava.lang.Object;")
    (element-type [m]
      java.lang.Object))

(extend-protocol mp/PIndexedAccess
  (Class/forName "[Ljava.lang.Object;")
    (get-1d [m x]
      (aget ^objects m (int x)))
    (get-2d [m x y]
      (mp/get-1d (aget ^objects m (int x)) y))
    (get-nd [m indexes]
      (let [^objects m m
            dims (long (count indexes))]
        (cond
          (== 1 dims)
            (aget m (int (first indexes)))
          (> dims 1) 
            (mp/get-nd (aget m (int (first indexes))) (next indexes)) 
          (== 0 dims) m
          :else
            (error "Invalid dimensionality access with index: " (vec indexes))))))

(extend-protocol mp/PIndexedSetting
  (Class/forName "[Ljava.lang.Object;")
    (set-1d [m x v]
      (let [^objects arr (copy-object-array m)]
        (aset arr (int x) v)
        arr))
    (set-2d [m x y v]
      (let [^objects arr (copy-object-array m)
            x (int x)]
        (aset arr x (mp/set-1d (aget ^objects m x) y v))
        arr))
    (set-nd [m indexes v]
      (let [dims (long (count indexes))]
        (cond 
          (== 1 dims)
            (let [^objects arr (copy-object-array m)
                  x (int (first indexes))]
              (aset arr (int x) v)
              arr)
          (> dims 1)
            (let [^objects arr (copy-object-array m)
                  x (int (first indexes))]
              (aset arr x (mp/set-nd (aget ^objects m x) (next indexes) v))
              arr)  
          :else 
            (error "Can't set on object array with dimensionality: " (count indexes)))))
    (is-mutable? [m] true))
