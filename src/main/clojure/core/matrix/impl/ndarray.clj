(ns core.matrix.impl.ndarray
  (:require [core.matrix.protocols :as mp])
  (:use core.matrix)
  (:use core.matrix.utils)
  (:require [core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Lightweight support for n-dimensional arrays of arbitrary objects conforming to core.matrix API 
;; 1D / 2D arrays of java.lang.Number can be safely used as vectors and matrices respectively

;; TODO: implementations of protocols for ND arrays

;; =======================================================
;; N-dimensional array object

(deftype NDArray 
  [^objects data
   ^longs dims]
  mp/PIndexedAccess
    (get-1d [m x]
      (aget data x))
    (get-2d [m x y]
      (let [ystride (long (aget dims 1))]
        (aget data (+ (long x) (* ystride (long y))))))
    (get-nd [m indexes]
      (let [ndims (count dims)
            index (areduce dims i result 0 
                           (+ (long (nth indexes i)) 
                              (if (> i 0) 
                                (* result (aget dims (dec i)))
                                0)))]
        (aget data index))) 
    
  mp/PDimensionInfo
    (dimensionality [m]
      (count dims))
    (dimension-count [m x]
      (aget dims x))
    )

(defn make-ndarray [dims]
  "Construct an NDArray with the specified dimensions. All values are initially null."
  (let [^longs dims (long-array dims)
        asize (areduce dims i result 1 (* result (aget dims i)))]
    (NDArray. (object-array asize)
              dims)))


;; =======================================================
;; N-dimensional view over an arbitrary core.matrix object

;; can represent slices, transposes etc.
;; can also allow in-place modification of original array

(deftype NDView
  [source
   ^longs dims
   ^longs strides
   ^long offset])

;; =======================================================
;; N-dimensional view over an array
;; 
;; like NDArrayView, but specialised for array source

(deftype NDArrayView
  [^objects data
   ^longs dims
   ^longs strides
   ^long offset]
  )
