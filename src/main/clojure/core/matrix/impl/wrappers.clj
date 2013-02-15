(ns clojure.core.matrix.impl.wrappers
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm]))

;; =============================================
;; SliceWrapper
;;
;; wraps a row-major slice of an array

(declare wrap-slice)

(deftype SliceWrapper [array ^long slice]
  mp/PImplementation
    (implementation-key [m] 
      :wrapper)
    ;; we delegate to persistent-vector implementation for new matrices.
    (new-vector [m length] 
      (mp/new-vector [] length))
    (new-matrix [m rows columns] 
      (mp/new-matrix [] rows columns))
    (new-matrix-nd [m dims] 
      (mp/new-matrix-nd [] dims))
    (construct-matrix [m data]
      (mp/construct-matrix [] data))
    (supports-dimensionality? [m dims] 
      true)
    
  mp/PDimensionInfo
    (dimensionality [m]
      (dec (mp/dimensionality array)))
    (get-shape [m]
      (next (mp/get-shape array)))
    (is-scalar? [m]
      false)
    (is-vector? [m]
      (== 2 (mp/dimensionality array)))
    (dimension-count [m dimension-number]
      (mp/dimension-count array (inc dimension-number)))
   
  mp/PIndexedAccess
    (get-1d [m row]
      (mp/get-2d array slice row))
    (get-2d [m row column]
      (mp/get-nd array [slice row column]))
    (get-nd [m indexes]
      (mp/get-nd array (cons slice indexes)))
    
  mp/PIndexedSetting
    (set-1d [m row v]
      (mp/set-2d array slice row v))
    (set-2d [m row column v]
      (mp/set-nd array [slice row column] v))
    (set-nd [m indexes v]
      (mp/set-nd array (cons slice indexes) v))
    (is-mutable? [m]
      (mp/is-mutable? array))
    
  mp/PMatrixCloning
    (clone [m] (wrap-slice (mp/clone array) slice)))
  

(defn wrap-slice [m slice]
  (SliceWrapper. m (long slice)))

(imp/register-implementation (wrap-slice [[1 2] [3 4]] 0))
