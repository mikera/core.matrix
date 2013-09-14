(ns clojure.core.matrix.impl.ndarray-macro
  (:require [clojure.pprint])
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.ndarray-magic :as magic])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro get-strided-idx
  "Returns an index inside of a strided array given a primitive long arrays
of indexes and strides"
  [strides offset idxs]
  `(+ (areduce ~idxs i# res# (int 0)
              (+ (* (aget ~idxs i#) (aget ~strides i#))
                 res#))
     ~offset))

(defmacro aget-nd
  [data strides offset idxs]
  `(let [idx# (get-strided-idx ~strides ~offset ~idxs)]
     (aget ~data idx#)))

(defmacro aset-nd
  [data strides offset idxs x]
  `(let [idx# (get-strided-idx ~strides ~offset ~idxs)]
     (aset ~data idx# ~x)))

(defmacro aget-2d
  [data strides offset i j]
  `(aget ~data
        (+ (+ (* (aget ~strides (int 0)) ~i)
              (* (aget ~strides (int 1)) ~j))
           ~offset)))

(defmacro aset-2d
  [data strides offset i j x]
  `(aset ~data
         (+ (+ (* (aget ~strides (int 0)) ~i)
               (* (aget ~strides (int 1)) ~j))
            ~offset)
         ~x))

(defmacro aadd-2d [data strides offset i j increment]
  `(let [idx# (+ (+ (* (aget ~strides (int 0)) ~i)
                    (* (aget ~strides (int 1)) ~j))
                 ~offset)]
     (aset ~data idx# (+ (aget ~data idx#) ~increment))))

(defn m-field [m-name field-name]
  (symbol (str m-name "-" field-name)))

(defn m-typed-field [m-name field-name type-name]
  (with-meta (m-field m-name field-name)
    {:tag type-name}))

(defmacro expose-ndarrays
  [matrices & body]
  (let [fields-gen (fn [m]
                     `[~(with-meta m {:tag 'typename#}) ~m
                       ~(m-typed-field m 'shape 'ints) (.shape ~m)
                       ~(m-typed-field m 'data 'array-tag#) (.data ~m)
                       ~(m-typed-field m 'strides 'ints) (.strides ~m)
                       ~(m-field m 'offset) (.offset ~m)
                       ~(m-field m 'ndims) (.ndims ~m)])
        bindings (mapcat fields-gen matrices)]
    `(let [~@bindings]
       ~@body)))

(defn unroll-predicate
  [pred xs]
  (case (count xs)
    0 true
    1 true
    2 `(~pred ~(first xs) ~(second xs))
    `(and (~pred ~(first xs) ~(second xs))
          ~(unroll-predicate pred (rest xs)))))

(defn loop-body-replace [body break-arg continue-arg]
  (clojure.walk/prewalk
   (fn [form]
     (if (and (seq? form) (= (count form) 2))
       (let [[op arg] form]
         (case op
           break (break-arg arg)
           continue (continue-arg arg)
           form))
       form))
   body))

(defmacro loop-over-0d-internal
  [[m1 & _ :as matrices] body]
  (let [idxs (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                     matrices)]
    `(let [~@idxs]
       ~body)))

(defmacro loop-over-1d-internal
  [[m1 & _ :as matrices] body]
  (let [init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                         matrices)
        recur (for [m matrices]
                    `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 0)))]
    `(loop [~'loop-i (int 0)
            ~@init]
       (when (< ~'loop-i (aget ~(m-field m1 'shape) 0))
         ~body
         (recur (inc ~'loop-i)
                ~@recur)))))

;; NOTE: this can be generalized to 3D, too
(defmacro loop-over-2d-internal
  [[m1 & _ :as matrices] body]
  (let [row-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                         matrices)
        col-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'idx)])
                         matrices)
        row-recur (for [m matrices]
                    `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 0)))
        col-recur (for [m matrices]
                    `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 1)))]
    `(loop [~'loop-row (int 0)
            ~@row-init]
       (when (< ~'loop-row (aget ~(m-field m1 'shape) 0))
         (loop [~'loop-col (int 0)
                ~@col-init]
           (when (< ~'loop-col (aget ~(m-field m1 'shape) 1))
             ~body
             (recur (inc ~'loop-col)
                    ~@col-recur)))
         (recur (inc ~'loop-row)
                ~@row-recur)))))

;; TODO: this can be done faster by adding strides in inner loop instead
;;       of calculating index with get-strided-idx
(defmacro loop-over-nd-internal
  [[m1 & _ :as matrices] body]
  (let [m-idxs (mapcat (fn [m] (let [m-strides (m-field m 'strides)
                                     m-offset (m-field m 'offset)]
                                 [(m-field m 'idx)
                                  `(get-strided-idx ~m-strides ~m-offset
                                                    ~'loop-idxs)]))
                       matrices)
        m1-ndims (m-field m1 'ndims)
        m1-shape (m-field m1 'shape)]
    `(loop [~'loop-idxs (int-array ~m1-ndims)]
       (let [~@m-idxs]
         ~body)
       (when (loop [dim# (int (dec ~m1-ndims))]
               (if (>= dim# 0)
                 (if (< (aget ~'loop-idxs dim#) (dec (aget ~m1-shape dim#)))
                   (do (aset ~'loop-idxs dim#
                             (inc (aget ~'loop-idxs dim#)))
                       true)
                   (do (aset ~'loop-idxs dim# (int 0))
                       (recur (dec dim#))))
                 false))
         (recur ~'loop-idxs)))))

;; TODO: use binding to ensure that it's inside magic
;; TODO: more docs here
;; TODO: introduce macro for current element retrieval
(defmacro loop-over
  "Helper macro for iterating over NDArray (or NDArrays) in efficient manner.
   Assumes that it's inside `with-magic` and all operands are of the same
   type (current 'magic' type) and shape; striding schemes can be different.
   Matrices argument should be a list of locals. Anaphoric arguments that
   can be used in a body given that [a, b] are provided: a-shape, b-shape,
   a-data, b-data, a-strides, b-strides, a-offset, b-offset, a-ndims, b-ndims,
   a-idx (current index into a-data), loop-i (if a and b are vectors;
   indexes inside vectors represented by a and b), loop-row, loop-col
   (if a and b are matrices); for higher dimensions loop-idxs will
   be available; a-step b-step; loop-acc"
  [[m1 & _ :as matrices] body]
  `(expose-ndarrays [~@matrices]
     (if-not ~(unroll-predicate 'java.util.Arrays/equals
                                (map #(m-field % 'shape) matrices))
       (iae "loop-over can iterate only over matrices of equal shape")
       (case ~(m-field m1 'ndims)
         0 (loop-over-0d-internal [~@matrices] ~body)
         1 (loop-over-1d-internal [~@matrices] ~body)
         2 (loop-over-2d-internal [~@matrices] ~body)
         (loop-over-nd-internal [~@matrices] ~body)))))

;; loop-over with accumulator
;; (loop [loop-row (int 0)
;;        a-idx a-offset
;;        b-idx b-offset
;;        c-idx c-offset
;;        acc nil]
;;   (if (< loop-row a-rows)
;;     (recur (inc loop-row)
;;            (+ a-idx (aget a-strides 0))
;;            (+ b-idx (aget b-strides 0))
;;            (+ c-idx (aget c-strides 0))
;;            (loop [loop-col (int 0)
;;                   a-idx a-idx
;;                   b-idx b-idx
;;                   c-idx c-idx
;;                   acc acc]
;;              (if (< loop-col a-cols)
;;                (recur (inc loop-col)
;;                       (+ a-idx (aget c-strides 1))
;;                       (+ b-idx (aget c-strides 1))
;;                       (+ c-idx (aget c-strides 1))
;;                       (aset c-data c-idx
;;                             (* (aget a-data a-idx)
;;                                (aget b-data b-idx))))
;;                acc)))
;;     acc))
