(ns core.matrix.impl.ndarray
  (:use core.matrix)
  (:require [core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

;; TODO: implementations of protocols

;; =======================================================
;; N-dimensional array object

(deftype NDArray 
  [^objects data
   ^ints dims]
  )


;; =======================================================
;; N-dimensional view over an arbitrary core.matrix object

;; can represent slices, transposes etc.
;; can also allow in-place modification of original array

(deftype NDView
  [source
   ^ints dims
   ^ints strides
   ^int offset])

;; =======================================================
;; N-dimensional view over an array
;; 
;; like NDArrayView, but specialised for array source

(deftype NDArrayView
  [^objects data
   ^ints dims
   ^ints strides
   ^int offset]
  )
