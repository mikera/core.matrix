(ns clojure.core.matrix.operators
  (:refer-clojure :exclude [* - + / vector? ==])
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.utils :refer [error]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; =====================================================================
;; Mathematical operators defined for numerical arrays as applicable

(defn *
  "Array multiply operator. Uses elementwise multiplication."
  ([a] a)
  ([a b]
    (m/mul a b))
  ([a b & more]
    (reduce m/mul (m/mul a b) more)))

(defn **
  "array exponent operator. Raises every element in matrix a to the given exponent.
   Equivalent to clojure.core.matrix/pow."
  ([a exponent]
    (m/pow a exponent)))

(defn +
  "Array addition operator. Equivalent to clojure.core.matrix/add."
  ([a] a)
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/+ a b)
      (m/add a b)))
  ([a b & more]
    (reduce + (+ a b) more)))

(defn -
  "Array subtraction operator. Equivalent to clojure.core.matrix/sub."
  ([a] (m/negate a))
  ([a b]
    (if (and (number? a) (number? b))
      (clojure.core/- a b)
      (m/sub a b)))
  ([a b & more]
    (reduce - (- a b) more)))

(defn /
  "Element-wise matrix division. Equivalent to clojure.core.matrix/div."
  ([a] (m/div a)) ;; this computes the reciprocal
  ([a b] (m/div a b))
  ([a b & more] (reduce m/div (m/div a b) more)))

(defn ==
  "Numerical array comparison. Equivalent to reducing with clojure.core.matrix/equals."
  ([] true)
  ([a] true)
  ([a b] (m/equals a b))
  ([a b & more] (reduce (fn [r m] (and r (== a m))) (== a b) more)))

(defmacro Σ
  "Computes array summation over a range of values for one or more variables"
  ([[sym vals & more :as bindings] exp]
    (cond
      (odd? (count bindings)) (error "Summation requires an even number of forms in binding vector")
      (seq more) `(Σ [~sym ~vals] (Σ [~@more] ~exp))
      :else `(reduce m/add (map (fn [i#] (let [~sym i#] ~exp)) ~vals)))))

(defmacro Π
  "Computes array products over a range of values for one or more variables"
  ([[sym vals & more :as bindings] exp]
    (cond
      (odd? (count bindings)) (error "Summation requires an even number of forms in binding vector")
      (seq more) `(Σ [~sym ~vals] (Σ [~@more] ~exp))
      :else `(reduce m/mul (map (fn [i#] (let [~sym i#] ~exp)) ~vals)))))

;; ===================================================
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
