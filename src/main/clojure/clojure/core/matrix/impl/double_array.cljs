(ns clojure.core.matrix.impl.double-array
  "Implementation supporting:

   - Javascript double[] arrays as core.matrix 1D vectors
  "
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.utils :as u])
  (:require-macros [clojure.core.matrix.macros :refer [scalar-coerce error is-double-array?]]))

(defn new-double-array
  "Creates a new zero-filled nested double array of the given shape"
  [shape]
  (let [dims (count shape)]
    (cond
      (== 0 dims) 0.0
      (== 1 dims) (double-array (int (first shape)))
      :else
        (let [ns (next shape)
              rn (long (first shape))
              r0 (new-double-array ns)]
          (into-array (cons r0 (for [i (range (dec rn))] (new-double-array ns))))))))

(defn construct-double-array [data]
  (let [dims (long (mp/dimensionality data))]
    (cond
     ;(== dims 2)
     ; (let [x (long (mp/dimension-count data 0))
     ;       y (long (mp/dimension-count data 1))
     ;       r (double-array (* x y))]
     ;   (dotimes [i x]
     ;     (dotimes [j y]
     ;       (aset-double r i j (double (mp/get-2d data i j)))))
     ;   r)
     (== dims 1)
       (let [n (long (mp/dimension-count data 0))
             r (double-array n)]
           (dotimes [i n]
             (aset r i (double (mp/get-1d data i))))
           r)
     (== dims 0)
       (double (mp/get-0d data))
     :default
       nil)))

(extend-protocol mp/PImmutableAssignment
  number
  (assign [m source] nil))

(extend-protocol mp/PImplementation
  array
  (implementation-key [m] :number-array)
  (meta-info [m]
    {:doc "Clojure.core.matrix implementation for JS arrays"})
  (new-vector [m length] (double-array (int length)))
  (new-matrix [m rows columns]
    (new-double-array [rows columns]))
  (new-matrix-nd [m shape]
    (new-double-array shape))
  (construct-matrix [m data]
    (let [dims (long (mp/dimensionality data))]
      (cond
        (== dims 2) (error "Double arrays in cljs only support a single dimension currently - submit a patch!")
        (== dims 1)
          (let [n (long (mp/dimension-count data 0))
                arr (double-array n)]
            (dotimes [i n]
              (aset arr i (double (mp/get-1d data i))))
            arr)
        (== dims 0)
          (double (mp/get-0d data))
        :default
          nil)))
  (supports-dimensionality? [m dims]
    (let [dims (long dims)]
      (or (== dims 1) (== dims 2)))))

(extend-protocol mp/PDimensionInfo
  array
  (dimensionality [m] 1)
  (is-vector? [m] true)
  (is-scalar? [m] false)
  (get-shape [m] (list (count m)))
  (dimension-count [m x]
    (if (== (long x) 0)
      (count m)
      (error "Double array does not have dimension: " x))))

(extend-protocol mp/PTypeInfo
  array
  (element-type [m] js/Number))

(extend-protocol mp/PIndexedAccess
  array
  (get-1d [m x] (aget m (int x)))
  (get-2d [m row column]
      (error "Can't do get-2D from 1D double array"))
  (get-nd [m indexes]
    (if (== 1 (count indexes))
      (aget m (int (first indexes)))
      (error "Can't get from double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PIndexedSetting
  array
  (set-1d [m x v]
    (let [arr (.slice m)]
      (aset arr (int x) v)
      arr))
  (set-2d [m x y v]
    (error "Can't do 2D set on double array"))
  (set-nd [m indexes v]
    (if (== 1 (count indexes))
      (let [arr (.slice m)
            x (int (first indexes))]
        (aset arr (int x) v)
        arr)
      (error "Can't set on double array with dimensionality: " (count indexes))))
  (is-mutable? [m] true))

(extend-protocol mp/PIndexedSettingMutable
  array
  (set-1d! [m x v]
    (aset m (int x) v))
  (set-2d! [m x y v]
    (error "Can't do 2D set on double array"))
  (set-nd! [m indexes v]
    (if (== 1 (count indexes))
      (aset m (int (first indexes)) v)
      (error "Can't set on double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PMutableMatrixConstruction
  array
  (mutable-matrix [m] (.slice m)))

(extend-protocol mp/PMatrixScaling
  array
  (scale [m a]
    (let [m m
          len (alength m)
          arr (double-array len)
          a (double a)]
      (dotimes [i len] (aset arr i (* a (aget m i))))
      arr))
  (pre-scale [m a]
    (let [m m
          len (alength m)
          arr (double-array len)
          a (double a)]
      (dotimes [i len] (aset arr i (* a (aget m i))))
      arr)))

(extend-protocol mp/PMatrixMutableScaling
  array
  (scale! [m a]
    (let [m m
          a (double a)]
      (dotimes [i (alength m)] (aset m i (* a (aget m i))))))

  (pre-scale! [m a]
    (let [m m
          a (double a)]
      (dotimes [i (alength m)] (aset m i (* a (aget m i)))))))

(extend-protocol mp/PConversion
  array
  (convert-to-nested-vectors [m]
    (vec m)))

(extend-protocol mp/PFunctionalOperations
  array
  (element-seq [m] m)

  (element-map
    ([m f]
     (let [cnt (alength m)
           r (double-array cnt)]
       (dotimes [i cnt]
         (aset r i (f (aget m i))))
       r))
    ([m f a]
     (let [r (double-array m)
           a (mp/broadcast-coerce r a)]
       (dotimes [i (alength m)]
         (aset r i (f (aget m i) (aget a i))))
       m))
    ([m f a more]
     (let [m (double-array m)
           a (mp/broadcast-coerce m a)
           more (mapv #(mp/broadcast-coerce m %) more)
           more-count (long (count more))
           vs (double-array more-count)]
       (dotimes [i (alength m)]
         (dotimes [j more-count] (aset vs j (aget (more j) i)))
         (aset m i (double (apply f (aget m i) (aget a i) vs))))
       m)))

  (element-map!
    ([m f]
     (mp/assign! m (mp/element-map m f)))
    ([m f a]
     (mp/assign! m (mp/element-map m f a)))
    ([m f a more]
     (mp/assign! m (mp/element-map m f a more))))

  (element-reduce
    ([m f]
     (let [m m]
       (reduce f m)))
    ([m f init]
     (let [m m]
       (reduce f init m)))))

(extend-protocol mp/PIndexImplementation
  array
	  (index? [m] true)
	  (index-to-longs [m] m)
	  (index-to-ints [m] m)
	  (index-from-longs [m xs]
      (long-array xs))
	  (index-from-ints [m xs]
      (int-array xs))
	  (index-coerce [m a]
      m))

(extend-protocol mp/PBroadcast
  array
  (broadcast [m new-shape]
    (let [nshape new-shape
          mshape (mp/get-shape m)
          mdims (count mshape)
          ndims (count nshape)]
      (cond
        (and (== mdims ndims) (u/same-shape-object? nshape mshape)) m
        :else (wrap/wrap-broadcast m new-shape)))))

(extend-protocol mp/PNumerical
  array
  (numerical? [m] true))

(extend-protocol mp/PSubVector
  array
  (subvector [m start length]
    (mp/subvector (wrap/wrap-nd m) start length)))

(extend-protocol mp/PMatrixEquality
  array
  (matrix-equals [a b]
    (cond
      (identical? a b) true
      (mp/same-shape? a b)
      (if (== 0 (long (mp/dimensionality a)))
        (== (mp/get-0d a) (scalar-coerce b))
        (not-any? false? (map == (mp/element-seq a) (mp/element-seq b))))
      :else false)))

(extend-protocol mp/PSameShape
  array
  (same-shape? [a b]
    (u/same-shape-object? (mp/get-shape a) (mp/get-shape b))))

(extend-protocol mp/PSelect
  array
  (select [a area]
    (or (mp/select-view a area)
        (wrap/wrap-selection a area))))

