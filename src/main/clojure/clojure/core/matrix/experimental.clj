(ns clojure.core.matrix.experimental
  (:use clojure.core.matrix))

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

