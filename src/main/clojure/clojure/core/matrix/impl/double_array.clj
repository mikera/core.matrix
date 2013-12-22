(ns clojure.core.matrix.impl.double-array
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
;; Useful as a fast, mutable 1D vector implementation.

(def ^:const DOUBLE-ARRAY-CLASS (Class/forName "[D"))

(def array-magic-data 
  {:double {:class DOUBLE-ARRAY-CLASS
            :regname :ndarray-double
            :fn-suffix 'double
            :array-tag 'doubles
            :array-cast 'double-array
            :type-cast 'double
            :type-object Double/TYPE}})

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

(extend-protocol mp/PImplementation
  (Class/forName "[D")
    (implementation-key [m] :double-array)
    (meta-info [m]
      {:doc "Clojure.core.matrix implementation for Java double arrays"})
    (new-vector [m length] (double-array (int length)))
    (new-matrix [m rows columns] (error "Can't make a 2D matrix from a double array"))
    (new-matrix-nd [m dims]
      (if (== 1 (count dims))
        (double-array (int (first dims)))
        (error "Can't make a double array of dimensionality: " (count dims))))
    (construct-matrix [m data]
      (construct-double-array data))
    (supports-dimensionality? [m dims]
      (== dims 1)))


(extend-protocol mp/PDimensionInfo
  (Class/forName "[D")
    (dimensionality [m] 1)
    (is-vector? [m] true)
    (is-scalar? [m] false)
    (get-shape [m] (list (count m)))
    (dimension-count [m x]
      (if (== (long x) 0)
        (count m)
        (error "Double array does not have dimension: " x))))

;; explicitly specify we use a primitive type
(extend-protocol mp/PTypeInfo
  (Class/forName "[D")
    (element-type [m]
      Double/TYPE))

(extend-protocol mp/PDoubleArrayOutput
  (Class/forName "[D")
    (to-double-array [m] (copy-double-array m))
    (as-double-array [m] m))

(extend-protocol mp/PObjectArrayOutput
  (Class/forName "[D")
    (to-object-array [m] (object-array m))
    (as-object-array [m] nil))

(extend-protocol mp/PIndexedAccess
  (Class/forName "[D")
    (get-1d [m x]
      (aget ^doubles m (int x)))
    (get-2d [m x y]
      (error "Can't do get-2D from 1D double array"))
    (get-nd [m indexes]
      (if (== 1 (count indexes))
        (aget ^doubles m (int (first indexes)))
        (error "Can't get from double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PSummable
  (Class/forName "[D")
    (element-sum [m]
      (let [^doubles m m]
        (areduce m i res 0.0 (+ res (aget m i))))))

(extend-protocol mp/PIndexedSetting
  (Class/forName "[D")
    (set-1d [m x v]
      (let [^doubles arr (copy-double-array m)]
        (aset arr (int x) (double v))
        arr))
    (set-2d [m x y v]
      (error "Can't do 2D set on double array"))
    (set-nd [m indexes v]
      (if (== 1 (count indexes))
        (let [^doubles arr (copy-double-array m)
              x (int (first indexes))]
          (aset arr (int x) (double v))
          arr)
        (error "Can't set on double array with dimensionality: " (count indexes))))
    (is-mutable? [m] true))

(extend-protocol mp/PIndexedSettingMutable
  (Class/forName "[D")
    (set-1d! [m x v]
      (aset ^doubles m (int x) (double v)))
    (set-2d! [m x y v]
      (error "Can't do 2D set on double array"))
    (set-nd! [m indexes v]
      (if (== 1 (count indexes))
        (aset ^doubles m (int (first indexes)) (double v))
        (error "Can't set on double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PMutableMatrixConstruction
  (Class/forName "[D")
    (mutable-matrix [m]
      (copy-double-array m)))

(extend-protocol mp/PMatrixScaling
  (Class/forName "[D")
    (scale [m a]
      (let [^doubles m m
            len (alength m)
            arr (double-array len)
            a (double a)]
        (dotimes [i len] (aset arr i (* a (aget m i))))
        arr))
    (pre-scale [m a]
      (let [^doubles m m
            len (alength m)
            arr (double-array len)
            a (double a)]
        (dotimes [i len] (aset arr i (* a (aget m i))))
        arr)))


(extend-protocol mp/PMatrixMutableScaling
  (Class/forName "[D")
    (scale! [m a]
      (let [^doubles m m
            a (double a)]
        (dotimes [i (alength m)] (aset m i (* a (aget m i))))))
    (pre-scale! [m a]
      (let [^doubles m m
            a (double a)]
        (dotimes [i (alength m)] (aset m i (* a (aget m i)))))))


(extend-protocol mp/PConversion
  (Class/forName "[D")
    (convert-to-nested-vectors [m]
      (vec m)))

(defmacro doubles-squared-sum [a]
  `(let [a# ~(vary-meta a assoc :tag 'doubles)
         n# (alength a#)]
     (loop [i# 0 res# 0.0]
       (if (< i# n#)
         (recur (inc i#) (+ res# (let [v# (aget a# i#)] (* v# v#))))
         res#))))

(extend-protocol mp/PVectorOps
  (Class/forName "[D")
    (vector-dot [a b] 
      (cond
        (is-double-array? b)
          (let [^doubles a a
                ^doubles b b
                n (alength a)]
            (when (not (== n (alength b))) (error "Incompatible double array lengths"))
            (loop [i 0 res 0.0]
              (if (< i n)
                (recur (inc i) (+ res (* (aget a i) (aget b i))))
                res)))
        (== 1 (mp/dimensionality b))
          (let [^doubles a a
                n (alength a)]
            (when (not (== n (mp/dimension-count b 0))) (error "Incompatible vector lengths"))
            (loop [i 0 res 0.0]
              (if (< i n)
                (recur (inc i) (+ res (* (aget a i) (mp/get-1d b i))))
                res)))
        :else nil))
    (length [a] (Math/sqrt (doubles-squared-sum a)))
    (length-squared [a] 
      (doubles-squared-sum a))
    (normalise [a]
      (let [a ^doubles a
            len (doubles-squared-sum a)]
        (cond
          (> len 0.0) (mp/scale a (/ 1.0 (Math/sqrt len))) 
          :else (double-array (alength a))))))

(extend-protocol mp/PCoercion
  (Class/forName "[D")
    (coerce-param [m param]
      (cond
        (is-double-array? param) param
        :else (construct-double-array param))))

(extend-protocol mp/PMatrixCloning
  (Class/forName "[D")
    (clone [m]
      (java.util.Arrays/copyOf ^doubles m (int (count m)))))

(extend-protocol mp/PFunctionalOperations
  (Class/forName "[D")
    (element-seq [m]
      (seq m))
    (element-map
      ([m f]
        (let [^doubles m (double-array m)]
          (dotimes [i (alength m)]
            (aset m i (double (f (aget m i)))))
          m))
      ([m f a]
        (let [^doubles m (double-array m)
              ^doubles a (mp/broadcast-coerce m a)]
          (dotimes [i (alength m)]
            (aset m i (double (f (aget m i) (aget a i)))))
          m))
      ([m f a more]
        (let [^doubles m (double-array m)
              ^doubles a (mp/broadcast-coerce m a)
              more (mapv #(mp/broadcast-coerce m %) more)
              more-count (long (count more))
              ^doubles vs (double-array more-count)]
          (dotimes [i (alength m)]
            (dotimes [j more-count] (aset vs j (aget ^doubles (more j) i)))
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
        (let [^doubles m m]
          (reduce f m)))
      ([m f init]
        (let [^doubles m m]
          (reduce f init m)))))

(extend-protocol mp/PMatrixDivideMutable
  (Class/forName "[D")
  (element-divide!
    ([m] (let [^doubles m m]
             (dotimes [i (alength m)]
               (aset m i (/ 1.0 / (aget m i))))
             nil))
    ([m a] (if (number? a)
             (let [^doubles m m]
               (dotimes [i (alength m)]
                 (aset m i (/ (aget m i) a))))
             (let [[^doubles m ^doubles a] (mp/broadcast-compatible m a)]
               (dotimes [i (alength m)]
                 (aset m i (/ (aget m i) (aget a i)))))))))

(extend-protocol mp/PMatrixDivide
  (Class/forName "[D")
  (element-divide
    ([m] (mp/element-map m #(/ %)))
    ([m a] (if (number? a)
             (mp/element-map m #(/ % a))
             (let [[m a] (mp/broadcast-compatible m a)]
               (mp/element-map m #(/ %1 %2) a))))))

;; registration

(imp/register-implementation (double-array [1]))
