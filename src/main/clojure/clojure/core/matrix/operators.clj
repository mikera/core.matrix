(ns clojure.core.matrix.operators
  (:refer-clojure :exclude [* - + / vector? ==])
  (:require [clojure.core.matrix :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =====================================================================
;; Mathematical operators defined for matrices and vectors as applicable

(defn *
  "Matrix multiply operator. Uses elementwise multiplication."
  ([a] a)
  ([a b]
    (m/mul a b))
  ([a b & more]
    (reduce m/mul (m/mul a b) more)))

;; TODO: remove the ^:dynamic once figured out way to stop "not declared dynamic" warning
(defn ^:dynamic **
  "Matrix exponent operator. Raises every element in matrix a to the given exponent.
   Uses clojure.core.matrix/pow."
  ([a exponent]
    (m/pow a exponent)))

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
  ([a] (m/negate a))
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/- a b)
      (m/sub a b)))
  ([a b & more]
    (reduce - (- a b) more)))

(defn /
  "Element-wise matrix division."
  ([a] (m/div a)) ;; this computes the reciprocal
  ([a b] (m/div a b))
  ([a b & more] (reduce m/div (m/div a b) more)))

(defn ==
  "Matrix numerical comparison. Performs == on an element-wise basis"
  ([] true)
  ([a] true)
  ([a b] (m/equals a b))
  ([a b & more] (reduce (fn [r m] (and r (== a m))) (== a b) more)))

;; inplace operators
(defn +=
  "Inplace matrix addition operator"
  ([a] a)
  ([a b] (m/add! a b)))

(defn -=
  "Inplace matrix subtraction operator"
  ([a] a)
  ([a b] (m/sub! a b)))

(defn *=
  "Inplace matrix multiplication operator"
  ([a] a)
  ([a b] (m/mul! a b)))

;; TODO can't use /= due to clojure namespace issues
(defn div=
  "Inplace matrix division operator"
  ([a] a)
  ([a b] (m/div! a b)))
