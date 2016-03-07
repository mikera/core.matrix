(ns clojure.core.matrix.operators
  "Namespace for core.matrix operators.

   These operators provide convenient notation for common array operations (e.g. +, *, -=) that
   replace equivalent functions in clojure.core and operate on whole arrays at once. Behaviour
   for numerical scalar values remians the same as clojure.core"
  (:refer-clojure :exclude [* - + / vector? == < <= > >= not= = min max])
  (:require [clojure.core.matrix :as m])
  (#?(:clj :require :cljs :require-macros)
           [clojure.core.matrix.macros :refer [error]]))

#?(:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
))

;; =====================================================================
;; Mathematical operators defined for numerical arrays as applicable

(def
  ^{:doc "Array multiply operator. Uses elementwise multiplication."}
  *
  m/mul)

(def **
  ^{:doc "Array exponent operator. Raises every element in matrix a to the given exponent.
   Equivalent to clojure.core.matrix/pow."}
  m/pow)

(defn +
  "Array addition operator. Equivalent to clojure.core.matrix/add."
  ([] (m/add))
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

;; comment these out for now, appear to be causing build issues, see #230
;
;(defmacro Σ
;  "Computes array summation over a range of values for one or more variables"
;  ([[sym vals & more :as bindings] exp]
;    (cond
;      (odd? (count bindings)) (error "Summation requires an even number of forms in binding vector")
;      (seq more) `(Σ [~sym ~vals] (Σ [~@more] ~exp))
;      :else `(reduce m/add (map (fn [i#] (let [~sym i#] ~exp)) ~vals)))))
;
;(defmacro Π
;  "Computes array products over a range of values for one or more variables"
;  ([[sym vals & more :as bindings] exp]
;    (cond
;      (odd? (count bindings)) (error "Summation requires an even number of forms in binding vector")
;      (seq more) `(Σ [~sym ~vals] (Σ [~@more] ~exp))
;      :else `(reduce m/mul (map (fn [i#] (let [~sym i#] ~exp)) ~vals)))))

;; ===================================================
;; inplace operators
(defn +=
  "Inplace matrix addition operator"
  ([a] a)
  ([a b]
    (m/add! a b))
  ([a b & more]
    (m/add! a b)
    (doseq [m more]
      (m/add! a m))))

(defn -=
  "Inplace matrix subtraction operator"
  ([a] a)
  ([a b]
    (m/sub! a b))
  ([a b & more]
    (m/sub! a b)
    (doseq [m more]
      (m/sub! a m))))

(defn *=
  "Inplace matrix multiplication operator"
  ([a] a)
  ([a b]
    (m/mul! a b))
  ([a b & more]
    (m/mul! a b)
    (doseq [m more]
      (m/mul! a m))))

;; TODO can't use /= due to clojure namespace issues
(defn div=
  "Inplace matrix division operator"
  ([a] a)
  ([a b] (m/div! a b))
  ([a b & more]
    (m/div! a b)
    (doseq [m more]
      (m/div! a m))))

;; =====================================================================
;; Comparison operators

(defn min
  "Computes the element-wise min of arrays"
  ([a]
    a)
  ([a b]
    (m/eif (m/sub a b) b a))
  ([a b & more]
    (reduce min (min a b) more)))

(defn max
  "Computes the element-wise max of arrays"
  ([a]
    a)
  ([a b]
    (m/eif (m/sub a b) a b))
  ([a b & more]
    (reduce min (min a b) more)))
