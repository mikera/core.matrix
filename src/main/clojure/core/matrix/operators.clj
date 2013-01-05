(ns core.matrix.operators
  (:require [core.matrix :as m])
  (:refer-clojure :exclude [* - + /]))

(defn *
  "Matrix multiply operator"
  ([a] a)
  ([a b]
    (m/mul a b))
  ([a b & more]
    (reduce m/mul (m/mul a b) more)))

(defn +
  "Matrix addition operator"
  ([a] a)
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/+ a b)
      (m/add a b)))
  ([a b & more]
    (reduce m/add (m/add a b) more)))

(defn -
  "Matrix subtraction operator"
  ([a] a)
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/- a b)
      (m/sub a b)))
  ([a b & more]
    (reduce m/sub (m/sub a b) more)))


