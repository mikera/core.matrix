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

; Utilities:

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
                         (count (aget m 0))))
    (dimension-count [m x]
      (condp == (long x)
        0 (count m)
        1 (count (aget m 0))
        (error "Double array does not have dimension: " x))))

;; explicitly specify we use a primitive type
(extend-protocol mp/PTypeInfo
  (Class/forName "[[D")
    (element-type [m]
      Double/TYPE))

(extend-protocol mp/PDoubleArrayOutput
  (Class/forName "[[D")
    (to-double-array [m]
      (into-array (Class/forName "[D")
                  (mapv copy-double-array m)))
    (as-double-array [m] m))

(extend-protocol mp/PObjectArrayOutput
  (Class/forName "[[D")
    (to-object-array [m] (into-array Object
                                     (mapv object-array m)))
    (as-object-array [m] nil))

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
      (let [m (copy-2d-double-array m)
            x (alength m)
            y (alength (aget m 0))]
        (dotimes [i x]
          (dotimes [j y]
            (aset-double m i j (* a (aget m i j)))))
        m))
    (pre-scale [m a]
      (let [m (copy-2d-double-array m)
            x (alength m)
            y (alength (aget m 0))]
        (dotimes [i x]
          (dotimes [j y]
            (aset-double m i j (* a ^double (aget m i j)))))
        m)))

(extend-protocol mp/PMatrixMutableScaling
  (Class/forName "[[D")
    (scale! [m a]
      (let [x (alength m)
            y (alength (aget m 0))]
        (dotimes [i x]
          (dotimes [j y]
            (aset-double m i j (* a ^double (aget m i j)))))))
    (pre-scale! [m a]
      (let [x (alength m)
            y (alength (aget m 0))]
        (dotimes [i x]
          (dotimes [j y]
            (aset-double m i j (* a ^double (aget m i j))))))))

(extend-protocol mp/PConversion
  (Class/forName "[[D")
    (convert-to-nested-vectors [m]
      (->> m
           (mapv vec)
           (mapv vec))))

; registration
(imp/register-implementation (make-array Double/TYPE 1 1))
