(ns clojure.core.matrix.impl.ndarray
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Lightweight support for n-dimensional arrays of arbitrary objects conforming to clojure.core.matrix API 
;; 1D / 2D arrays of java.lang.Number can be safely used as vectors and matrices respectively

;; utility functions

(defn ^long calc-total-size [^longs dims]
  (let [c (count dims)]
    (loop [i (long 0) result (long 1)]
      (if (>= 0 c)
        result
        (recur (inc i) (* result (aget dims i)))))))

;; =======================================================
;; N-dimensional array object

(deftype NDArray 
  [^objects data
   ^longs dims]
  mp/PImplementation
    (implementation-key [m] :ndarray)
    (new-vector [m length] (TODO))
    (new-matrix [m rows columns] (TODO))
    (new-matrix-nd [m dims] (TODO))
    (construct-matrix [m data] (TODO))
    (supports-dimensionality? [m dims] true)
  
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
    (get-shape [m]
      dims)
    (is-vector? [m]
      (== 1 (count dims))) 
    (is-scalar? [m]
      false) ;; TODO: what about zero dimension case??
    (dimensionality [m]
      (count dims))
    (dimension-count [m x]
      (aget dims x))
    
  mp/PIndexedSetting
    (set-1d [m x v]
      (aset data x v))
    (set-2d [m x y v]
      (let [ystride (long (aget dims 1))]
        (aset data (+ (long x) (* ystride (long y))) v)))
    (set-nd [m indexes v]
      (let [ndims (count dims)
            index (areduce dims i result 0 
                           (+ (long (nth indexes i)) 
                              (if (> i 0) 
                                (* result (aget dims (dec i)))
                                0)))]
        (aget data index v)))
    
    ;; TODO: implementations of other protocols for ND arrays
    )

(defn make-ndarray [dims]
  "Construct an NDArray with the specified dimensions. All values are initially null."
  (let [^longs dims (long-array dims)
        asize (areduce dims i result 1 (* result (aget dims i)))]
    (NDArray. (object-array asize)
              dims)))


;; =======================================================
;; N-dimensional view over an arbitrary clojure.core.matrix object

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
  
   ;; TODO: implementations of other protocols for ND array views
  )

;; =====================================
;; Register implementation

(imp/register-implementation (make-ndarray [1]))