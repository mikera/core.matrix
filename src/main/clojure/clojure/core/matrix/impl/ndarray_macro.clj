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

;; ## Helpers
;;
;; This functions will be used everywhere in this file to generate symbols.

(defn m-field [m-name field-name]
  (symbol (str m-name "-" field-name)))

(defn m-typed-field [m-name field-name type-name]
  (with-meta (m-field m-name field-name)
    {:tag type-name}))

;; A function to find an index inside of strided array.
;; General formula for finding an element of given index inside of a
;; strided array is
;; $$index = (n\_1, n\_2, \dots d\_N)$$
;; $$offset = \sum\_{i=0}^{N-1} s\_i n\_i$$
;; (see [[py2]])

(defmacro get-strided-idx
  "Returns an index inside of a strided array given a primitive long arrays
of indexes and strides"
  [strides offset idxs]
  `(+ (areduce ~idxs i# res# (int 0)
              (+ (* (aget ~idxs i#) (aget ~strides i#))
                 res#))
     ~offset))

;; ## Getters and setters
;;
;; Functions with names with * at the end assume that they are used inside
;; `expose-ndarrays`.

(defmacro aget-nd
  "Like aget, but for n-dimensional NDArray"
  [data strides offset idxs]
  `(let [idx# (get-strided-idx ~strides ~offset ~idxs)]
     (aget ~data idx#)))

(defmacro aset-nd
  "Like aset, but for n-dimensional NDArray"
  [data strides offset idxs x]
  `(let [idx# (get-strided-idx ~strides ~offset ~idxs)]
     (aset ~data idx# ~x)))

(defmacro aget-1d
  "Like aget, but for 1-dimensional NDArray"
  [data strides offset i]
  `(aget ~data (+ (* (aget ~strides (int 0)) ~i)
                  ~offset)))

(defmacro aget-1d*
  "Like aget, but for 1-dimensional NDArray. Assumes that it's inside
   `expose-ndarrays`"
  [m i]
  `(aget-1d ~(m-field m 'data) ~(m-field m 'strides) ~(m-field m 'offset)
            ~i))

(defmacro aset-1d
  "Like aset, but for 1-dimensional NDArray"
  [data strides offset i x]
  `(aset ~data (+ (* (aget ~strides (int 0)) ~i)
                  ~offset)
         ~x))

(defmacro aset-1d*
  "Like aset, but for 1-dimensional NDArray. Assumes that it's inside
   `expose-ndarrays`"
  [m i x]
  `(aset-1d ~(m-field m 'data) ~(m-field m 'strides) ~(m-field m 'offset)
            ~i ~x))

(defmacro aget-2d
  "Like aget, but for 2-dimensional NDArray"
  [data strides offset i j]
  `(aget ~data
        (+ (+ (* (aget ~strides (int 0)) ~i)
              (* (aget ~strides (int 1)) ~j))
           ~offset)))

(defmacro aget-2d*
  "Like aget, but for 2-dimensional NDArray. Assumes that it's inside
   `expose-ndarrays`"
  [m i j]
  `(aget-2d ~(m-field m 'data) ~(m-field m 'strides) ~(m-field m 'offset)
            ~i ~j))

(defmacro aset-2d
  "Like aset, but for 2-dimensional NDArray"
  [data strides offset i j x]
  `(aset ~data
         (+ (+ (* (aget ~strides (int 0)) ~i)
               (* (aget ~strides (int 1)) ~j))
            ~offset)
         ~x))

(defmacro aset-2d*
  "Like aset, but for 2-dimensional NDArray. Assumes that it's inside
   `expose-ndarrays`"
  [m i j x]
  `(aset-2d ~(m-field m 'data) ~(m-field m 'strides) ~(m-field m 'offset)
            ~i ~j ~x))

(defmacro aadd-2d
  "Increments value inside NDArray by a given amount"
  [data strides offset i j increment]
  `(let [idx# (+ (+ (* (aget ~strides (int 0)) ~i)
                    (* (aget ~strides (int 1)) ~j))
                 ~offset)]
     (aset ~data idx# (+ (aget ~data idx#) ~increment))))

;; ## Generally useful stuff

(defmacro expose-ndarrays
  "An anaphoric macro that takes a list of names of NDArrays and provides
   properly hinted bindings for NDArray fields. For example,
   `(expose-ndarrays [a b])` will bind names `a-shape`, `b-shape`, `a-offset`
   and so on in it's body"
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
  "Generates code for unrolling a binary predicate over a list of provided
   symbols. For example, `(unroll-predicate 'pred ['a 'b 'c])` will return
   `'(and (pred a b) (pred b c))`"
  [pred xs]
  (case (count xs)
    0 true
    1 true
    2 `(~pred ~(first xs) ~(second xs))
    `(and (~pred ~(first xs) ~(second xs))
          ~(unroll-predicate pred (rest xs)))))

;; ## loop-over and it's internals
;;
;; This is a bunch of special cases for loop-over. Most of the time it will
;; be used over 1d or 2d arrays, so it makes sense to provided special faster
;; code for this cases.

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

;; In general case we iterate over n-dimensional array using a primitive int
;; array of current indices into NDArray.

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

(defmacro loop-over
  "Helper macro for iterating over NDArray (or NDArrays) in efficient manner.
   Assumes that it's inside `with-magic` and all operands are of the same
   type (current 'magic' type) and shape; striding schemes can be different.
   Matrices argument should be a list of locals. Anaphoric arguments that
   can be used in a body given that [a, b] are provided: a-shape, b-shape,
   a-data, b-data, a-strides, b-strides, a-offset, b-offset, a-ndims, b-ndims,
   a-idx, b-idx (current indexes into a and b)"
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

;; ## fold-over and it's internals
;;
;; fold-over is similar to loop-over, but allows to pass an accumulator
;; through the loop. In some tests it's substantially slower (1-3x) than
;; loop-over, I don't know why.

(defmacro fold-over-0d-internal
  [[m1 & _ :as matrices] init body]
  (let [loop-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                     matrices)
        loop-recur (for [m matrices]
                `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 0)))]
    `(let [~'loop-i (int 0)
           ~'loop-acc (~'type-cast# ~init)
           ~@loop-init]
       ~body)))

(defmacro fold-over-1d-internal
  [[m1 & _ :as matrices] init body]
  (let [loop-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                     matrices)
        loop-recur (for [m matrices]
                `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 0)))]
    `(loop [~'loop-i (int 0)
            ~'loop-acc (~'type-cast# ~init)
            ~@loop-init]
       (if (< ~'loop-i (aget ~(m-field m1 'shape) 0))
         (recur (inc ~'loop-i)
                ~body
                ~@loop-recur)
         ~'loop-acc))))

(defmacro fold-over-2d-internal
  [[m1 & _ :as matrices] init body]
  (let [row-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'offset)])
                         matrices)
        col-init (mapcat (fn [m] [(m-field m 'idx) (m-field m 'idx)])
                         matrices)
        row-recur (for [m matrices]
                    `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 0)))
        col-recur (for [m matrices]
                    `(+ ~(m-field m 'idx) (aget ~(m-field m 'strides) 1)))]
    `(loop [~'loop-row (int 0)
            ~@row-init
            ~'loop-acc (~'type-cast# ~init)]
       (if (< ~'loop-row (aget ~(m-field m1 'shape) 0))
         (recur (inc ~'loop-row)
                ~@row-recur
                (~'type-cast#
                 (loop [~'loop-col (int 0)
                        ~@col-init
                        ~'loop-acc ~'loop-acc]
                   (if (< ~'loop-col (aget ~(m-field m1 'shape) 1))
                     (recur (inc ~'loop-col)
                            ~@col-recur
                            (~'type-cast# ~body))
                     ~'loop-acc))))
         ~'loop-acc))))

(defmacro fold-over
  "Helper macro for iterating over NDArray (or NDArrays) in efficient manner
   using an accumulator. Assumes that it's inside `with-magic` and all
   operands are of the same type (current 'magic' type) and shape; striding
   schemes can be different. Matrices argument should be a list of locals.
   Anaphoric arguments that can be used in a body given that [a, b] are
   provided: a-shape, b-shape, a-data, b-data, a-strides, b-strides, a-offset,
   b-offset, a-ndims, b-ndims,  a-idx, b-idx (current indices into a and b),
   loop-acc (current value of accumulator). The passed body should return a
   new value of accumulator; it will be cast to current 'magic' type"
  [[m1 & _ :as matrices] init body]
  `(expose-ndarrays [~@matrices]
     (if-not ~(unroll-predicate 'java.util.Arrays/equals
                                (map #(m-field % 'shape) matrices))
       (iae "fold-over can iterate only over matrices of equal shape")
       (case ~(m-field m1 'ndims)
         0 (fold-over-0d-internal [~@matrices] ~init ~body)
         1 (fold-over-1d-internal [~@matrices] ~init ~body)
         2 (fold-over-2d-internal [~@matrices] ~init ~body)
         (TODO))))) ;;  "NDArray fold-over macro requires implementation for 3+ dimension case"
