(ns clojure.core.matrix.experimental
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.protocols :as mp]))

;; Namespace for experimental functions and macros, building on core.matrix

(defmacro matrix-for
  "Constructs a matrix by iterating the bindings over the given shape.

   Example: (matrix-for [[i j] [3 3]] (+ i j))"
  ([[bindings shape] & body]
    (if (seq bindings)
      `(mapv
         (fn [~(first bindings)] (matrix-for [~(next bindings) ~(next shape)] ~@body))
         (range ~(first shape)))
      `(do ~@body))))

(defn raw-shape 
  "Returns the raw shape of a matrix. May be any type that suports seq, including Java arrays.
   The advantage of this methods is that it avoids converting the shape to a Clojure vector
   (the behaviour of shape)."
  ([m]
    (mp/get-shape m)))

