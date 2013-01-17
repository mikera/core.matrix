(ns core.matrix.impl.double-array
  (:require [core.matrix.protocols :as mp])
  (:use core.matrix)
  (:use core.matrix.utils)
  (:require core.matrix.impl.persistent-vector)
  (:require [core.matrix.implementations :as imp])
  (:require [core.matrix.impl.mathsops :as mops])
  (:require [core.matrix.multimethods :as mm]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


;; core.matrix implementation for Java double arrays
;;
;; Useful as a fast, mutable 1D vector implementation. Not good for much else.


(extend-protocol mp/PImplementation
  (Class/forName "[D")
    (implementation-key [m] :double-array)
    (new-vector [m length] (double-array (int length)))
    (new-matrix [m rows columns] (error "Can't make a 2D matrix from a double array"))
    (new-matrix-nd [m dims] 
      (if (== 1 (count dims)) 
        (double-array (int (first dims)))
        (error "Can't make a double array of dimensionality: " (count dims))))
    (construct-matrix [m data]
      (cond 
        (mp/is-scalar? data) 
          data
        (== (dimensionality data) 1) 
          (double-array (eseq data))
        :default
          (error "Don't know how to construct double array from " (class data))))
    (supports-dimensionality? [m dims]
      (<= dims 1)))

(extend-protocol mp/PIndexedAccess
  (Class/forName "[D")
    (get-1d [m x]
      (aget ^doubles m (int x)))
    (get-2d [m x y]
      (error "Can't do 2D get from double array"))
    (get-nd [m indexes]
      (if (== 1 (count indexes)) 
        (aget ^doubles m (int (first indexes)))
        (error "Can't get from double array with dimensionality: " (count indexes)))))


(extend-protocol mp/PConversion
  (Class/forName "[D")
    (convert-to-nested-vectors [m]
      (vec m)))


(extend-protocol mp/PDimensionInfo
  (Class/forName "[D")
    (dimensionality [m] 1)
    (is-vector? [m] true)
    (is-scalar? [m] false)
    (dimension-count [m x]
      (if (== x 0)
        (count m)
        (error "Double array does not have dimension: " x))))

;; registration

(imp/register-implementation (double-array [1]))

