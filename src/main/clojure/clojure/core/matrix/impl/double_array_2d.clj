(ns clojure.core.matrix.impl.double-array-2d
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :refer :all]
            [clojure.core.matrix.impl.double-array 
             :refer [new-double-array construct-double-array
                     to-double-arrays]]))

; 2D double array implementation

; TODO : Is it safe to assume that all columns have the
; same length?

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

; Utilities:

(defmacro is-2d-double-array? [m]
  `(instance? ~(Class/forName "[[D") ~m))

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

(defn ^"[[D" copy-2d-double-array [^"[[D" m]
  (into-array (Class/forName "[D")
              (mapv copy-double-array ^"[[D" m)))

; Protocol implementations
(extend-protocol mp/PImplementation
  (Class/forName "[[D")
    (implementation-key [m] :double-array-2d)
    (meta-info [m]
      {:doc "Clojure.core.matrix implementation for 2D Java double arrays"})
    (new-vector [m length] 
      (double-array (int length)))
    (new-matrix [m rows columns] 
      (new-double-array [rows columns]))
    (new-matrix-nd [m shape]
      (new-double-array shape))
    (construct-matrix [m data]
      (construct-double-array data))
    (supports-dimensionality? [m dims]
      (== dims 2)))

(extend-protocol mp/PDimensionInfo
  (Class/forName "[[D")
    (dimensionality [m] 2)
    (is-vector? [m] false)
    (is-scalar? [m] false)
    (get-shape [m] (list (count m)
                         (count (aget ^"[[D" m 0))))
    (dimension-count [m x]
      (condp == (long x)
        0 (count m)
        1 (count (aget ^"[[D" m 0))
        (error "Double array does not have dimension: " x))))

;; explicitly specify we use a primitive type
(extend-protocol mp/PTypeInfo
  (Class/forName "[[D")
    (element-type [m]
      Double/TYPE))

(comment
  (extend-protocol mp/PDoubleArrayOutput
    (Class/forName "[[D")
      (to-double-array [m]
        (let [[x y] (mp/get-shape m)
              res (make-array Double/TYPE x y)]
          (dotimes [i x]
            (dotimes [j y]
              (aset-double res i j (double (aget m i j)))))
          res))
      (as-double-array [m] m))


  (extend-protocol mp/PObjectArrayOutput
    (Class/forName "[[D")
      (to-object-array [m] (into-array (Class/forName "[Ljava.lang.Object;")
                                       (mapv object-array m)))
      (as-object-array [m] nil)))

(extend-protocol mp/PSummable
  (Class/forName "[[D")
    (element-sum [m]
      (let [^"[[D" m m]
        (areduce m i res-outer 0.0
          (+ res-outer
             (let [^doubles a (aget m i)]
               (areduce a j res-inner 0.0
                        (+ res-inner (aget a j)))))))))

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
  (Class/forName "[[D")
    (mutable-matrix [m]
      (copy-2d-double-array m)))

(extend-protocol mp/PMatrixScaling
  (Class/forName "[[D")
    (scale [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            ^"[[D" res (make-array Double/TYPE x y)]
        (dotimes [i x]
          (let [^doubles res-i (aget res i)
                ^doubles m-i (aget ^"[[D" m i)]
            (dotimes [j y]
              (aset-double res-i j (* a (aget m-i j))))))
        res))
    (pre-scale [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))
            ^"[[D" res (make-array Double/TYPE x y)]
        (dotimes [i x]
          (let [^doubles res-i (aget res i)
                ^doubles m-i (aget ^"[[D" m i)]
            (dotimes [j y]
              (aset-double res-i j (* a (aget m-i j))))))
        res)))

(extend-protocol mp/PMatrixMutableScaling
  (Class/forName "[[D")
    (scale! [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))]
        (loop-over-2d
          m i j
          (aset-double m-i j (* a (double (aget m-i j)))))))
    (pre-scale! [m a]
      (let [x (alength ^"[[D" m)
            y (alength ^doubles (aget ^"[[D" m 0))]
        (loop-over-2d
          m i j
          (aset-double m-i j (* a (double (aget m-i j))))))))

(extend-protocol mp/PConversion
  (Class/forName "[[D")
    (convert-to-nested-vectors [m]
      (->> m
           (mapv vec)
           (mapv vec))))

(extend-protocol mp/PVectorOps
  (Class/forName "[[D")
    (vector-dot [m b]
      (cond
        (== 1 (mp/dimensionality b))
        (let [x (mp/dimension-count m 0)
              ^doubles final-results (double-array x)
              ^"[[D" m m]
          (loop [i 0]
            (if (< i x)
              (do
                (aset-double final-results i
                             (let [^doubles a (aget m i)]
                               (areduce a j res 0.0
                                        (+ res (* (mp/get-1d b j) 
                                         (aget a j))))))
                (recur (inc i))) 
              final-results)))
        (== 2 (mp/dimensionality b))
        (let [x (mp/dimension-count m 0)
              ^doubles final-results (double-array x)]
          (loop [i 0]
            (if (< i x)
              (do
                (aset-double final-results i
                             (let [^doubles a (aget ^"[[D" m i)]
                               (areduce a j res 0.0
                                        (+ res (* (mp/get-2d b i j)
                                                  (aget a j))))))
                (recur (inc i)))
              final-results)))
        :else nil)))

(extend-protocol mp/PCoercion
  (Class/forName "[[D")
    (coerce-param [m param]
      (cond
        (is-2d-double-array? param) param
        :else (construct-double-array param))))


(extend-protocol mp/PMatrixCloning
  (Class/forName "[[D")
    (clone [m]
      (java.util.Arrays/copyOf ^"[[D" m (int (alength ^"[[D" m)))))

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

(extend-protocol mp/PMatrixDivide
  (Class/forName "[[D")
  (element-divide
    ([m] (mp/element-map m #(/ %)))
    ([m a] (if (number? a)
             (mp/element-map m #(/ % a))
             (mp/element-map m #(/ %1 %2) a)))))

(extend-protocol mp/PMatrixDivideMutable
  (Class/forName "[[D")
  (element-divide!
    ([^"[[D" m]
     (loop-over-2d
       m i j
       (aset-double ^"[[D" m-i j (double (/ 1.0 ^double (aget m-i j))))))
    ([^"[[D" m a]
     (if (number? a)
       (loop-over-2d
         m i j
         (aset-double ^"[[D" m-i j (double (/ ^double (aget m-i j) a))))
       (loop-over-2d
         m i j
         (aset-double ^"[[D" m-i j (double (/ ^double (aget m-i j)
                                              ^double (aget a i j)))))))))

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

; registration
(imp/register-implementation (make-array Double/TYPE 1 1))
