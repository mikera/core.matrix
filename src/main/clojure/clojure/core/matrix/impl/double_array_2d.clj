(ns clojure.core.matrix.impl.double-array-2d
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :refer :all]
            [clojure.core.matrix.impl.double-array 
             :refer [new-double-array construct-double-array
                     to-double-arrays]]))

; 2D double array implementation
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

(imp/register-implementation (make-array (Class/forName "[D") 1 1))

