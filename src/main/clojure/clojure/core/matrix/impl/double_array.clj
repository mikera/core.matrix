(ns clojure.core.matrix.impl.double-array
  "Implementation supporting:
   
   - Java double[] arrays as core.matrix 1D vectors
   - Java double[][] arrays as core.matrix 2D matrices"
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; clojure.core.matrix implementation for Java double arrays
;;
;; Useful as a fast, mutable 1D vector implementation or 2D matrix implementation.

(def ^:const DOUBLE-ARRAY-CLASS (Class/forName "[D"))

(def array-magic-data
  {:double {:class DOUBLE-ARRAY-CLASS
            :regname :ndarray-double
            :fn-suffix 'double
            :array-tag 'doubles
            :array-cast 'double-array
            :type-cast 'double
            :type-object Double/TYPE}})

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
              ^Object r0 (new-double-array ns)]
          (into-array (.getClass r0) (cons r0 (for [i (range (dec rn))] (new-double-array ns))))))))

(defn construct-double-array [data]
  (let [dims (long (mp/dimensionality data))]
    (cond
     (== dims 2)
      (let [x (long (mp/dimension-count data 0))
            y (long (mp/dimension-count data 1))
            r (make-array Double/TYPE x y)]
        (dotimes [i x]
          (dotimes [j y]
            (aset-double r i j (double (mp/get-2d data i j)))))
        r)
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


(defn to-double-arrays
  "Converts an array to nested double arrays with the same shape."
  [m]
  (if-let [dims (long (mp/dimensionality m))]
    (cond 
      (== 0 dims) (double (mp/get-0d m))
      (== 1 dims) (mp/to-double-array m)
      :else (let [r0 (to-double-arrays (mp/get-major-slice m 0))
                  c (.getClass ^Object r0)]
              (into-array c (map to-double-arrays (mp/get-major-slice-seq m)))))))

(defn ^"[[D" copy-2d-double-array [^"[[D" m]
  (into-array (Class/forName "[D")
              (mapv copy-double-array ^"[[D" m)))

(defmacro loop-over-2d
  "Defines a convinient way to loop through 2D Java arrays, binding i and j
  to the indices of the row and column respectively. It also binds the current row
  to the symbol m-{i} (in an optimized manner).

  This macro should only be used when there is no row-specific computation."
  ([m i j & body]
   (let [row-symbol (symbol (str "m-" (name i)))]
     `(let [[x# y#] (mp/get-shape ^"[[D" ~m)
            ~(with-meta 'm {:tag "[[D"}) ~m]
       (dotimes [~i x#]
         (let [~(with-meta row-symbol {:tag 'doubles}) (aget ~'m ~i)]
           (dotimes [~j y#]
             ~@body)))))))

(defmacro is-2d-double-array? [m]
  `(instance? ~(Class/forName "[[D") ~m))

(defmacro defimplementation
  "Defines a new implementaiton for a N-D java array"
  ([klass]
   `(extend-protocol mp/PImplementation
      ~klass
      (implementation-key [m#] :double-array)
      (meta-info [m#]
        {:doc "Clojure.core.matrix implementation for Java double arrays"})
      (new-vector [m# length#] (double-array (int length#)))
      (new-matrix [m# rows# columns#] 
        (new-double-array [rows# columns#]))
      (new-matrix-nd [m# shape#]
        (new-double-array shape#))
      (construct-matrix [m# data#]
        (construct-double-array data#))
      (supports-dimensionality? [m# dims#]
        (let [dims# (long dims#)]
          (or (== dims# 1) (== dims# 2)))))))

(defimplementation (Class/forName "[D"))
(defimplementation (Class/forName "[[D"))

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

(extend-protocol mp/PDimensionInfo
  (Class/forName "[[D")
    (dimensionality [m] 2)
    (is-vector? [m] false)
    (is-scalar? [m] false)
    (get-shape [m] (list (count m)
                         (count (aget ^"[[D" m 0))))
    (dimension-count [m x]
      (condp == (long x)
        0 (alength ^"[[D" m)
        1 (alength ^doubles (aget ^"[[D" m 0))
        (error "Double array does not have dimension: " x))))

;; explicitly specify we use a primitive type
(extend-protocol mp/PTypeInfo
  (Class/forName "[D")
    (element-type [m]
      Double/TYPE))

(extend-protocol mp/PTypeInfo
  (Class/forName "[[D")
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

(extend-protocol mp/PDoubleArrayOutput
  (Class/forName "[[D")
    (to-double-array [m]
      (let [[^long rows ^long cols] (mp/get-shape m)
            ^doubles res (double-array (* rows cols))]
        (dotimes [i rows]
          (let [^doubles row (aget ^"[[D" m i)]
            (dotimes [j cols]
              (aset res (+ j (* i cols)) (aget row j)))))
        res))
    (as-double-array [m] nil))

(extend-protocol mp/PObjectArrayOutput
  (Class/forName "[[D")
    (to-object-array [m] 
      (let [[^long rows ^long cols] (mp/get-shape m)
            ^"[Ljava.lang.Object;" res (object-array (* rows cols))]
        (dotimes [i rows]
          (let [^doubles row (aget ^"[[D" m i)]
            (dotimes [j cols]
              ;; TODO: fix identity hack that is needed to fix reflection warning
              (aset res (+ j (* i cols)) (identity (aget row j))))))
        res))
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

(extend-protocol mp/PIndexedAccess
  (Class/forName "[[D")
    (get-1d [m x]
      (error "Cannot do get-1d from a 2D double array"))
    (get-2d [m x y]
      ^double (aget ^"[[D" m x y))
    (get-nd [m indexes]
      (if (== 2 (count indexes))
        (let [[x y] indexes]
          ^double (aget ^"[[D" m (int x) (int y)))
        (error "Can't get from double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PSummable
  (Class/forName "[D")
    (element-sum [m]
      (let [^doubles m m]
        (areduce m i res 0.0 (+ res (aget m i))))))

(extend-protocol mp/PSummable
  (Class/forName "[[D")
    (element-sum [m]
      (let [^"[[D" m m]
        (areduce m i res-outer 0.0
          (+ res-outer
             (let [^doubles a (aget m i)]
               (double (areduce a j res-inner 0.0
                               (+ res-inner (aget a j))))))))))

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

(extend-protocol mp/PIndexedSetting
  (Class/forName "[[D")
    (set-1d [m x v]
      (error "Can't do 1D set on 2D double array"))
    (set-2d [m x y v]
      (let [^"[[D" mat (copy-2d-double-array m)]
        (aset-double mat x y (double v))
        mat))
    (set-nd [m indexes v]
      (if (== (count indexes) 2)
        (let [^"[[D" mat (copy-2d-double-array m)
              [x y] indexes]
          (aset-double mat x y (double v))
          mat) 
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

(extend-protocol mp/PIndexedSettingMutable
  (Class/forName "[[D")
    (set-1d! [m x v]
      (error "Can't do 1D set on 2D double array"))
    (set-2d! [m x y v]
      (aset-double m x y (double v)))
    (set-nd! [m indexes v]
      (if (== (count indexes) 2)
        (let [[x y] indexes]
          (aset-double m x y (double v)))
        (error "Can't set on double array with dimensionality: " (count indexes)))))

(extend-protocol mp/PMutableMatrixConstruction
  (Class/forName "[D")
    (mutable-matrix [m]
      (copy-double-array m)))

(extend-protocol mp/PMutableMatrixConstruction
  (Class/forName "[[D")
    (mutable-matrix [m]
      (copy-2d-double-array m)))

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

(extend-protocol mp/PMatrixScaling
  (Class/forName "[[D")
    (scale [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            ^"[[D" res (make-array Double/TYPE x y)
            a (double a)]
        (dotimes [i x]
          (let [^doubles res-i (aget res i)
                ^doubles m-i (aget ^"[[D" m i)]
            (dotimes [j y]
              (aset res-i j (* a (aget m-i j))))))
        res))
    (pre-scale [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            ^"[[D" res (make-array Double/TYPE x y)
            a (double a)]
        (dotimes [i x]
          (let [^doubles res-i (aget res i)
                ^doubles m-i (aget ^"[[D" m i)]
            (dotimes [j y]
              (aset res-i j (* a (aget m-i j))))))
        res)))

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

(extend-protocol mp/PMatrixMutableScaling
  (Class/forName "[[D")
    (scale! [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            a (double a)]
        (loop-over-2d
          m i j
          (aset m-i j (* a (double (aget m-i j)))))))
    (pre-scale! [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            a (double a)]
        (loop-over-2d
          m i j
          (aset m-i j (* a (double (aget m-i j))))))))

(extend-protocol mp/PConversion
  (Class/forName "[D")
    (convert-to-nested-vectors [m]
      (vec m)))

(extend-protocol mp/PConversion
  (Class/forName "[[D")
    (convert-to-nested-vectors [m]
      (->> m
           (mapv vec)
           (mapv vec))))

(defmacro doubles-squared-sum [a]
  `(let [a# ~(vary-meta a assoc :tag 'doubles)
         n# (long (alength a#))]
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
                n (long (alength a))]
            (when-not (== n (alength b)) (error "Incompatible double array lengths"))
            (loop [i 0 res 0.0]
              (if (< i n)
                (recur (inc i) (+ res (* (aget a i) (aget b i))))
                res)))
        (== 1 (long (mp/dimensionality b)))
          (let [^doubles a a
                n (alength a)]
            (when (not (== n (long (mp/dimension-count b 0)))) (error "Incompatible vector lengths"))
            (loop [i 0 res 0.0]
              (if (< i n)
                (recur (inc i) (+ res (* (aget a i) (double (mp/get-1d b i)))))
                res)))
        :else nil))
    (length [a] (Math/sqrt (doubles-squared-sum a)))
    (length-squared [a]
      (doubles-squared-sum a))
    (normalise [a]
      (let [a ^doubles a
            len (double (doubles-squared-sum a))]
        (cond
          (> len 0.0) (mp/scale a (/ 1.0 (Math/sqrt len)))
          :else (double-array (alength a))))))

(extend-protocol mp/PVectorOps
  (Class/forName "[[D")
    (vector-dot [m b]
      (let [bdims (long (mp/dimensionality b))
            len (long (mp/dimension-count m 0))]
        (cond
         (== 1 bdims)
           (let [^doubles final-results (double-array len)
                 ^"[[D" m m
                 ^doubles b (to-double-arrays b)]
             (loop [i 0]
               (if (< i len)
                 (do
                   (aset-double final-results i
                                (let [^doubles a (aget m i)]
                                  (areduce a j res 0.0
                                           (+ res (* (aget b j) 
                                            (aget a j))))))
                   (recur (inc i))) 
                 final-results)))
         (== 2 bdims)
           (let [^doubles final-results (double-array len)]
             (loop [i 0]
               (if (< i len)
                 (do
                   (aset-double final-results i
                                (let [^doubles a (aget ^"[[D" m i)]
                                  (areduce a j res 0.0
                                           (+ res (* (double (mp/get-2d b i j))
                                                     (aget a j))))))
                   (recur (inc i)))
                 final-results)))
         :else nil))))

(extend-protocol mp/PCoercion
  (Class/forName "[D")
    (coerce-param [m param]
      (cond
        (is-double-array? param) param
        :else (construct-double-array param))))

(extend-protocol mp/PCoercion
  (Class/forName "[[D")
    (coerce-param [m param]
      (cond
        (is-2d-double-array? param) param
        :else (construct-double-array param))))
(extend-protocol mp/PCoercion
  (Class/forName "[[D")
    (coerce-param [m param]
      (cond
        (is-2d-double-array? param) param
        :else (construct-double-array param))))

(extend-protocol mp/PMatrixCloning
  (Class/forName "[D")
    (clone [m]
      (java.util.Arrays/copyOf ^doubles m (int (count m)))))

(extend-protocol mp/PMatrixCloning
  (Class/forName "[[D")
    (clone [m]
      (let [m ^"[[D" m
            len (int (alength ^"[[D" m)) 
            ^"[[D" res (make-array (Class/forName "[D") len)]
        (dotimes [i len]
          (let [^doubles arr (aget m i)]
            (aset res i (java.util.Arrays/copyOf arr (int (alength arr))))))
        res)))

(extend-protocol mp/PFunctionalOperations
  (Class/forName "[D")
    (element-seq [m]
      m)
    (element-map
      ([m f]
        (let [m ^doubles m
              cnt (alength m)
              ^doubles r (double-array cnt)]
          (dotimes [i cnt]
            (aset r i (double (f (aget m i)))))
          r))
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

(extend-protocol mp/PFunctionalOperations
  (Class/forName "[[D")
  (element-seq [m]
    (seq (apply concat m)))
  (element-map
    ([m f]
      (let [[x y] (mp/get-shape m)
            ^"[[D" res (make-array Double/TYPE x y)]
        (dotimes [i x]
          (let [^doubles m-i (aget ^"[[D" m i)
                ^doubles res-i (aget res i)]
            (dotimes [j y]
              (aset-double res-i j (double (f (aget m-i j)))))))
        res))
    ([m f a]
      (let [[x y] (mp/get-shape m)
            ^"[[D" a (mp/broadcast-coerce m a)
            ^"[[D" res (make-array Double/TYPE x y)]
        (dotimes [i x]
          (let [^doubles m-i (aget ^"[[D" m i)
                ^doubles res-i (aget res i)
                ^doubles a-i (aget ^"[[D" a i)]
            (dotimes [j y]
              (aset-double res-i j (double (f (aget m-i j) (aget a-i j)))))))
        res))
    ([m f a more]
     (let [[x y] (mp/get-shape m)
           ^"[[D" a (mp/broadcast-coerce m a)
           ^"[[D" res (make-array Double/TYPE x y) 
           more (mapv #(mp/broadcast-coerce m %) more)
           more-count (count more)
           ^doubles vs (double-array more-count)]
       (dotimes [i x]
         (let [^doubles m-i (aget ^"[[D" m i)
               ^doubles a-i (aget ^"[[D" m i)
               ^doubles res-i (aget ^"[[D" res i)
               more-i (mapv #(aget ^"[[D" % i) more)]
           (dotimes [j y]
             (dotimes [k more-count] (aset ^doubles vs k (double (aget ^doubles (more-i k) j))))
             (aset-double res-i j (double (apply f (aget m i j) (aget a i j) vs))))))
       res)))
  (element-map!
    ([m f]
      (mp/assign! m (mp/element-map m f)))
    ([m f a]
      (mp/assign! m (mp/element-map m f a)))
    ([m f a more]
      (mp/assign! m (mp/element-map m f a more))))
  (element-reduce
    ([m f]
     (mp/element-reduce m f 0))
    ([m f init]
      (let [[x y] (mp/get-shape m)]
        (areduce ^"[[D" m i res-outer (double init)
                 (let [^doubles arr (aget ^"[[D" m i)]
                   (double
                     (areduce ^doubles arr j res-inner res-outer
                              (double (f res-inner (aget arr j)))))))))))

(extend-protocol mp/PMapIndexed
  (Class/forName "[D")
    (element-map-indexed
      ([m f]
        (let [m ^doubles m
              cnt (alength m)
              ^doubles r (double-array cnt)]
          (dotimes [i cnt]
            (aset r i (double (f [i] (aget m i)))))
          r))
      ([m f a]
        (let [^doubles m (double-array m)
              ^doubles a (mp/broadcast-coerce m a)]
          (dotimes [i (alength m)]
            (aset m i (double (f [i] (aget m i) (aget a i)))))
          m))
      ([m f a more]
        (let [^doubles m (double-array m)
              ^doubles a (mp/broadcast-coerce m a)
              more (mapv #(mp/broadcast-coerce m %) more)
              more-count (long (count more))
              ^doubles vs (double-array more-count)]
          (dotimes [i (alength m)]
            (dotimes [j more-count] (aset vs j (aget ^doubles (more j) i)))
            (aset m i (double (apply f [i] (aget m i) (aget a i) vs))))
          m)))
    (element-map-indexed!
      ([m f]
        (mp/assign! m (mp/element-map-indexed m f)))
      ([m f a]
        (mp/assign! m (mp/element-map-indexed m f a)))
      ([m f a more]
        (mp/assign! m (mp/element-map-indexed m f a more)))))

(extend-protocol mp/PMapIndexed
  (Class/forName "[[D")
  (element-map-indexed
    ([m f]
      (let [[x y] (mp/get-shape m)
            ^"[[D" res (make-array Double/TYPE x y)]
        (dotimes [i x]
          (let [^doubles m-i (aget ^"[[D" m i)
                ^doubles res-i (aget res i)]
            (dotimes [j y]
              (aset-double res-i j (double (f [i j] (aget m-i j)))))))
        res))
    ([m f a]
     (let [[x y] (mp/get-shape m)
           ^"[[D" a (mp/broadcast-coerce m a)
           ^"[[D" res (make-array Double/TYPE x y)]
       (dotimes [i x]
         (let [^doubles m-i (aget ^"[[D" m i)
               ^doubles res-i (aget res i)
               ^doubles a-i (aget ^"[[D" a i)]
           (dotimes [j y]
             (aset-double res-i j (double (f [i j] (aget m-i j)))))))
       res))
    ([m f a more]
     (let [[x y] (mp/get-shape m)
           ^"[[D" a (mp/broadcast-coerce m a)
           ^"[[D" res (make-array Double/TYPE x y)
           more (mapv #(mp/broadcast-coerce m %) more)
           more-count (long (count more))
           ^doubles vs (double-array more-count)]
       (dotimes [i x]
         (let [^doubles m-i (aget ^"[[D" m i)
               ^doubles a-i (aget ^"[[D" m i)
               ^doubles res-i (aget ^"[[D" res i)
               more-i (mapv #(aget ^"[[D" % i) more)]
           (dotimes [j y]
             (dotimes [k more-count] (aset ^doubles vs k (double (aget ^doubles (more-i k) j))))
             (aset-double res-i j (double (apply f [i j]
                                                 (aget m i j)
                                                 (aget a i j)
                                                 vs))))))
       res)))
  (element-map-indexed!
    ([m f]
     (mp/assign! m (mp/element-map-indexed m f)))
    ([m f a]
     (mp/assign! m (mp/element-map-indexed m f a)))
    ([m f a more]
     (mp/assign! m (mp/element-map-indexed m f a more)))))

(extend-protocol mp/PMatrixDivideMutable
  (Class/forName "[D")
  (element-divide!
    ([m] (let [^doubles m m]
             (dotimes [i (alength m)]
               (aset m i (/ 1.0 (aget m i))))
             nil))
    ([m a] (if (number? a)
             (let [a (double a)]
               (let [^doubles m m]
                 (dotimes [i (long (alength m))]
                   (aset m i (/ (aget m i) a)))))
             (let [[^doubles m ^doubles a] (mp/broadcast-compatible m a)]
               (dotimes [i (long (alength m))]
                 (aset m i (/ (aget m i) (aget a i)))))))))

(extend-protocol mp/PMatrixDivideMutable
  (Class/forName "[[D")
  (element-divide!
    ([^"[[D" m]
     (loop-over-2d
       m i j
       (aset-double ^"[[D" m-i j (double (/ 1.0 ^double (aget m-i j))))))
    ([^"[[D" m a]
     (if (number? a)
       (let [a (double a)]
         (loop-over-2d
           m i j
           (aset-double ^"[[D" m-i j (double (/ ^double (aget m-i j) a)))))
       (loop-over-2d
         m i j
         (aset-double ^"[[D" m-i j (double (/ ^double (aget m-i j)
                                              ^double (aget a i j)))))))))

(extend-protocol mp/PMatrixDivide
  (Class/forName "[D")
  (element-divide
    ([m] (mp/element-map m #(/ %)))
    ([m a] (if (number? a)
             (let [a (double a)]
               (mp/element-map m #(/ (double %) a)))
             (let [[m a] (mp/broadcast-compatible m a)]
               (mp/element-map m / a))))))

(extend-protocol mp/PMatrixDivide
  (Class/forName "[[D")
  (element-divide
    ([m] (mp/element-map m #(/ %)))
    ([m a] (if (number? a)
             (let [a (double a)] (mp/element-map m #(/ (double %) a)))
             (mp/element-map m / a)))))

(extend-protocol mp/PSelect
  (Class/forName "[D")
  (select
    [m args]
    (if (= 1 (count args))
      (let [indices (vec (first args))
            ^doubles res (make-array Double/TYPE (count indices))]
        (dotimes [i (count indices)]
          (aset-double res i
                       ^double (aget ^doubles m (nth indices i))))
        res)
      (error "Cannot select " (count args) "dimension(s) on 1D double array"))))

(extend-protocol mp/PSelect
  (Class/forName "[[D")
  (select [m [x y :as args]]
    (if (= 2 (count args))
      (let [x (vec x)
            y (vec y)
            count-x (count x)
            count-y (count y)
            ^"[[D" res (make-array Double/TYPE count-x count-y)]
        (dotimes [i count-x]
          (let [^doubles m-i (aget ^"[[D" m (x i))
                ^doubles res-i (aget ^"[[D" res (x i))]
            (dotimes [j count-y]
              (aset-double res-i j
                           (aget m-i (y j)))))) 
        res)
      (error "select on 2D double array takes only 2 arguments"))))

;; registration

(imp/register-implementation (double-array [1]))
