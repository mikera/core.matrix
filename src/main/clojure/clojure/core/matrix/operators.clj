(ns clojure.core.matrix.operators
  (:refer-clojure :exclude [* - + / vector? ==])
  (:require [clojure.core.matrix :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =====================================================================
;; Mathematical operators defined for matrices and vectors as applicable

(defn *
  "Matrix multiply operator. Uses the inner product for multiplication."
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
    (reduce + (+ a b) more)))

(defn -
  "Matrix subtraction operator"
  ([a] a)
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/- a b)
      (m/sub a b)))
  ([a b & more]
    (reduce - (- a b) more)))

(defn /
  "Element-wise matrix division."
  ([a] a)
  ([a b] (m/div a b))
  ([a b & more] (reduce m/div (m/div a b) more)))

(defn ==
  "Matrix numerical comparison. Performs == on an element-wise basis"
  ([] true)
  ([a] true)
  ([a b] (m/equals a b))
  ([a b & more] (reduce (fn [r m] (and r (== a m))) (== a b) more)))
