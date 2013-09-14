(ns clojure.core.matrix.impl.ndarray-macro
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

(defmacro loop-over-1d
  [[m1 & _ :as matrices] init body]
  `(expose-ndarrays [~@matrices]
     (if-not ~(unroll-predicate 'java.util.Arrays/equals
                                (map #(m-field % 'shape) matrices))
       (iae "loop-over can iterate only over matrices of equal shape")
       (loop-over-1d-internal [~@matrices ~init ~body]))))

(defmacro loop-over-1d-internal
  [[m1 & _ :as matrices] init body]
  (let [steps-1d (mapcat (fn [m] [(m-field m 'step)
                                  `(aget ~(m-field m 'strides) (int 0))])
                         matrices)
        loop-init-1d (mapcat (fn [m] [(m-field m 'idx)
                                      (m-field m 'offset)])
                             matrices)
        loop-step-1d (mapcat
                      (fn [m] [`(+ ~(m-field m 'idx)
                                   ~(m-field m 'step))])
                      matrices)
        break-arg identity
        continue-arg (fn [arg] `(recur ~@loop-step-1d (inc ~'loop-i) ~arg))
        body-1d (loop-body-replace body break-arg continue-arg)]
    `(let [~@steps-1d
           end# (+ ~(m-field m1 'offset)
                   (* (aget ~(m-field m1 'shape) (int 0))
                      ~(m-field m1 'step)))]
       (loop [~@loop-init-1d
              ~'loop-i 0
              ~'loop-acc ~init]
         (if (< ~(m-field m1 'idx) end#)
           ~body-1d
           ~'loop-acc)))))

(defmacro loop-over-2d
  [[m1 & _ :as matrices] init body]
  `(expose-ndarrays [~@matrices]
     (if-not ~(unroll-predicate 'java.util.Arrays/equals
                                (map #(m-field % 'shape) matrices))
       (iae "loop-over can iterate only over matrices of equal shape")
       (loop-over-2d-internal [~@matrices] ~init ~body))))

#_(defmacro loop-over-2d-internal
  [[m1 & _ :as matrices] init body]
  (let [steps (mapcat (fn [m] [(m-field m 'step-col)
                               `(aget ~(m-field m 'strides) 1)
                               (m-field m 'step-row)
                               `(- (aget ~(m-field m 'strides) 0)
                                   (* (aget ~(m-field m 'strides) 1)
                                      (dec (aget ~(m-field m 'shape) 1))))])
                      matrices)
        loop-init (mapcat (fn [m] [(m-field m 'idx)
                                      (m-field m 'offset)])
                             matrices)
        break-arg identity
        continue-arg (fn [arg]
                       (let [make-step-col
                             (map (fn [m] `(+ ~(m-field m 'idx)
                                              ~(m-field m 'step-col)))
                                  matrices)
                             make-step-row
                             (map (fn [m] `(+ ~(m-field m 'idx)
                                              ~(m-field m 'step-row)))
                                  matrices)]
                         `(if (< ~'loop-col (dec ~'loop-ncols))
                            (recur ~@make-step-col
                                   ~'loop-row
                                   (inc ~'loop-col)
                                   ~arg
                                   (inc ~'i))
                            (recur ~@make-step-row
                                   (inc ~'loop-row)
                                   0
                                   ~arg
                                   (inc ~'i)))))
        body (loop-body-replace body break-arg continue-arg)]
    `(let [~@steps
           ~'loop-nrows (aget ~(m-field m1 'shape) 0)
           ~'loop-ncols (aget ~(m-field m1 'shape) 1)]
       (loop [~@loop-init
              ~'loop-row 0
              ~'loop-col 0
              ~'loop-acc ~init
              ~'i 0]
         (if (< ~'i 250) #_(and (< ~'loop-col ~'loop-ncols)
                       (< ~'loop-row ~'loop-nrows))
           ~body
           ~'loop-acc)))))

;; TODO: this should be faster
(defmacro loop-over-2d-internal
  [[m1 & _ :as matrices] init body]
  (let [steps (mapcat (fn [m] [(m-field m 'step-col)
                               `(aget ~(m-field m 'strides) 1)
                               (m-field m 'step-row)
                               `(- (aget ~(m-field m 'strides) 0)
                                   (* (aget ~(m-field m 'strides) 1)
                                      (dec (aget ~(m-field m 'shape) 1))))])
                      matrices)
        loop-init (mapcat (fn [m] [(m-field m 'idx)
                                      (m-field m 'offset)])
                             matrices)
        break-arg identity
        continue-arg (fn [arg]
                       (let [make-step-col
                             (map (fn [m] `(+ ~(m-field m 'idx)
                                              ~(m-field m 'step-col)))
                                  matrices)
                             make-step-row
                             (map (fn [m] `(+ ~(m-field m 'idx)
                                              ~(m-field m 'step-row)))
                                  matrices)]
                         `(if (< ~'loop-col (dec ~'loop-ncols))
                            (recur ~@make-step-col
                                   ~'loop-row
                                   (inc ~'loop-col)
                                   ~arg)
                            (recur ~@make-step-row
                                   (inc ~'loop-row)
                                   0
                                   ~arg))))
        body (loop-body-replace body break-arg continue-arg)]
    `(let [~@steps
           ~'loop-nrows (aget ~(m-field m1 'shape) 0)
           ~'loop-ncols (aget ~(m-field m1 'shape) 1)]
       (loop [~@loop-init
              ~'loop-row 0
              ~'loop-col 0
              ~'loop-acc ~init]
         (if (and (< ~'loop-col ~'loop-ncols)
                       (< ~'loop-row ~'loop-nrows))
           ~body
           ~'loop-acc)))))

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
  [[m1 & _ :as matrices] init body]
  `(expose-ndarrays [~@matrices]
     (if-not ~(unroll-predicate 'java.util.Arrays/equals
                                (map #(m-field % 'shape) matrices))
       (iae "loop-over can iterate only over matrices of equal shape")
       (case ~(m-field m1 'ndims)
         0 (TODO)
         1 (loop-over-1d-internal [~@matrices] ~init ~body)
         2 (loop-over-2d-internal [~@matrices] ~init ~body)
         ;;N-dimensional case
         (TODO)))))
