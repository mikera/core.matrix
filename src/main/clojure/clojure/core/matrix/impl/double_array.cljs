(ns clojure.core.matrix.impl.double-array
  "Implementation supporting:

   - Javascript double[] arrays as core.matrix 1D vectors
  "
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.wrappers :as wrap]
            [clojure.core.matrix.impl.common :refer [logistic-fn softplus-fn relu-fn
                                                     square? symmetric-matrix-entries?]]
            [clojure.core.matrix.utils :as u])
  (:require-macros [clojure.core.matrix.macros :refer [scalar-coerce error is-double-array? c-for]]))

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
  array
  (assign [m source] ))

(extend-protocol mp/PAssignment
  array
  (assign! [m source]
    (dotimes [i (count source)]
      (mp/set-1d! m i (nth source i)))
    m)
  (assign-array! [m arr start length]
    (let [length (long length)
          start (long start)]
      (dotimes [i length]
        (mp/set-1d! m i (nth arr (+ start i)))))
    m))

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

(extend-protocol mp/PReshaping
  array
  (reshape [m shape]
    (if (= (mp/get-shape m) shape) ;; Short circuit if already the desired shape
      m
      (let [gv (mp/generic-value m) ;; generic value for array padding. Typically nil or zero
            es (concat (mp/element-seq m) (repeat gv))
            partition-shape (fn partition-shape [es shape]
                              (if-let [s (seq shape)]
                                (let [ns (next s)
                                      plen (reduce * 1 ns)]
                                  (map #(partition-shape % ns) (partition plen es)))
                                (first es)))]
        (if-let [shape (seq shape)]
          (let [fs (long (first shape))
                parts (partition-shape es shape)]
            (or
              (mp/construct-matrix m (take fs parts))
              (mp/construct-matrix [] (take fs parts))))
          (first es))))))

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
    (aset m (int x) v)
    m)
  (set-2d! [m x y v]
    (error "Can't do 2D set on double array"))
  (set-nd! [m indexes v]
    (if (== 1 (count indexes))
      (do
        (aset m (int (first indexes)) v)
        m)
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
       r))
    ([m f a more]
     (let [r (double-array m)
           a (mp/broadcast-coerce m a)
           more (mapv #(mp/broadcast-coerce m %) more)
           more-count (long (count more))
           vs (double-array more-count)]
       (dotimes [i (alength m)]
         (dotimes [j more-count] (aset vs j (aget (more j) i)))
         (aset r i (double (apply f (aget m i) (aget a i) vs))))
       r)))

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

(extend-protocol mp/PLogistic
  array
  (logistic [m]
      (mp/element-map m logistic-fn)))

(extend-protocol mp/PLogisticMutable
  array
  (logistic! [m]
    (mp/element-map! m logistic-fn)))

(extend-protocol mp/PSoftplus
  array
  (softplus [m]
    (mp/element-map m softplus-fn)))

(extend-protocol mp/PSoftmax
  array
  (softmax [m]
    (let [em (mp/exp m)]
      (mp/element-divide em (mp/element-sum em)))))

(extend-protocol mp/PSoftmaxMutable
  array
  (softmax! [m]
    (mp/exp! m)
    (mp/element-divide! m (mp/element-sum m))
    m))

(extend-protocol mp/PSoftplusMutable
  array
  (softplus! [m]
    (mp/element-map! m softplus-fn)))

(extend-protocol mp/PReLU
  array
  (relu [m]
    (mp/element-map m relu-fn)))

(extend-protocol mp/PReLUMutable
  array
  (relu! [m]
    (mp/element-map! m relu-fn)))

(extend-protocol mp/PMatrixPredicates
  array
  (identity-matrix? [m]
    (let [rc (long (mp/dimension-count m 0))
          cc (long (mp/dimension-count m 1))]
      (if (and (== (long (mp/dimensionality m)) 2) (== rc cc))
        (loop [i (long 0)]
          (if (< i rc)
            (if (loop [j (long 0)]
                  (if (< j cc)
                    (let [elem (mp/get-2d m i j)]
                      (if (number? elem)
                        (if (== i j)
                          (if (== (double elem) 1.0) (recur (inc j)) false)
                          (if (zero? elem) (recur (inc j)) false))
                        false))
                    true))
              (recur (inc i))
              false)
            true))
        false)))
  (zero-matrix? [m]
    (every? #(and (number? %) (zero? %)) (mp/element-seq m)))
  (symmetric? [m]
    (case (long (mp/dimensionality m))
      0 true
      1 true
      2 (and (square? m) (symmetric-matrix-entries? m))
      (= m (mp/transpose m))))

  number
  (identity-matrix? [m] (= 1 m))
  (zero-matrix? [m] (zero? m))
  (symmetric? [m] true))

(extend-protocol mp/PSummable
  array
  (element-sum [a]
    (mp/element-reduce a (if (mp/numerical? a) + mp/matrix-add))))

(extend-protocol mp/PMatrixMultiply
  array
  (matrix-multiply [m a]
    (let [mdims (long (mp/dimensionality m))
          adims (long (mp/dimensionality a))]
      (cond
        (== adims 0) (mp/scale m a)
        (and (== mdims 1) (== adims 1)) (mp/vector-dot m a)
        (and (== mdims 1) (== adims 2))
        (let [[arows acols] (mp/get-shape a)]
          (mp/reshape (mp/matrix-multiply (mp/reshape m [1 arows]) a)
                      [acols]))
        (and (== mdims 2) (== adims 1))
        (let [[mrows mcols] (mp/get-shape m)]
          (mp/reshape (mp/matrix-multiply m (mp/reshape a [mcols 1]))
                      [mcols]))
        (and (== mdims 2) (== adims 2))
        (let [mutable (mp/is-mutable? m)
              [^long mrows ^long mcols] (mp/get-shape m)
              [^long arows ^long acols] (mp/get-shape a)
              new-m-type (if mutable m (imp/get-canonical-object :ndarray))
              new-m (mp/new-matrix new-m-type mrows acols)]
          ;; TODO: optimize cache-locality (http://bit.ly/12FgFbl)
          (c-for [i (long 0) (< i mrows) (inc i)
                  j (long 0) (< j acols) (inc j)]
                 (mp/set-2d! new-m i j 0))
          (c-for [i (long 0) (< i mrows) (inc i)
                  j (long 0) (< j acols) (inc j)
                  k (long 0) (< k mcols) (inc k)]
                 (mp/set-2d! new-m i j (+ (mp/get-2d new-m i j)
                                          (* (mp/get-2d m i k)
                                             (mp/get-2d a k j)))))
          new-m))))
  (element-multiply [m a]
    (if (number? a)
      (mp/scale m a)
      (let [[m a] (mp/broadcast-compatible m a)]
        (mp/element-map m clojure.core/* a)))))

(extend-protocol mp/PMatrixMultiplyMutable
  array
  (element-multiply! [m a]
    (mp/assign! m (mp/element-multiply m a)))
  (matrix-multiply! [m a]
    (mp/assign! m (mp/matrix-multiply m a))))

(extend-protocol mp/PMatrixDivide
  array
  (element-divide
    ([m]
     (if (mp/get-shape m)
       (mp/element-map m mp/element-divide)
       (error "Don't know how to take reciprocal of " (type m))))
    ([m a]
     (mp/element-multiply m (mp/element-divide a)))))

(extend-protocol mp/PMatrixDivideMutable
  array
  (element-divide!
    ([m] (mp/element-map! m /))
    ([m a]
     (let [[m a] (mp/broadcast-compatible m a)]
       (mp/element-map! m / a)))))

(extend-protocol mp/PBroadcastCoerce
  array
  (broadcast-coerce [m a]
    (mp/coerce-param m (mp/broadcast-like m a))))

(extend-protocol mp/PCoercion
  array
  (coerce-param [m param]
    (let [param (if (instance? ISeq param) (mp/convert-to-nested-vectors param) param)]
      (or (mp/construct-matrix m param)
          param))))

(extend-protocol mp/PBroadcastLike
  array
  (broadcast-like [m a]
    (let [sm (mp/get-shape m) sa (mp/get-shape a)]
      (if (u/same-shape-object? sm sa)
        a
        (mp/broadcast a sm)))))

(extend-protocol mp/PMatrixOps
  array
  (trace [m]
    (when-not (== 2 (long (mp/dimensionality m))) (error "Trace requires a 2D matrix"))
    (let [rc (long (mp/dimension-count m 0))
          cc (long (mp/dimension-count m 1))
          dims (Math/min rc cc)]
      (loop [i 0 res 0.0]
        (if (>= i dims)
          res
          (recur (inc i) (+ res (double (mp/get-2d m i i))))))))

  (determinant [m] nil)
  (inverse [m] nil))


