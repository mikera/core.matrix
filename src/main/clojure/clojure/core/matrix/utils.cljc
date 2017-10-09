(ns clojure.core.matrix.utils
  "Namespace for core.matrix utilities. Intended mainly for library and tool writers."
  (:refer-clojure :exclude [update])
  #?@(:clj [(:require [clojure.reflect :as r])
            (:import [java.util Arrays]
                     [clojure.lang IPersistentVector])])
  (#?(:clj :require :cljs :require-macros)
           [clojure.core.matrix.macros :refer [TODO is-long-array?]]))

;; Some of these are copies of methods from the library
;;   https://github.com/mikera/clojure-utils
;;
;; duplicated here to avoid an extra dependency

#?(:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
)
:cljs (do
(def class type)
))

;; this duplicates clojure.core.matrix.utils.macros
(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (#?(:clj RuntimeException.
                :cljs js/Error.)
                     (str ~@vals)))))

(defmacro doseq-indexed
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
  `(loop [vals# (seq ~values)
          ~index-sym (long 0)]
     (if vals#
       (let [~val-sym (first vals#)]
             ~@code
             (recur (next vals#) (inc ~index-sym)))
       nil))))

; this duplicates clojure.core.matrix.utils.macros
;; however needed to allow some legacy code to run
#?(:clj 
    (defmacro error?
  "Returns true if executing body throws an error, false otherwise."
  ([& body]
    `(try
       ~@body
       false
       (catch #?(:clj Throwable :cljs js/Error) t#
         true)))))

;;; this duplicates clojure.core.matrix.utils.macros
;(defmacro doseq-indexed
;  "loops over a set of values, binding index-sym to the 0-based index of each value"
;  ([[val-sym values index-sym] & code]
;  `(loop [vals# (seq ~values)
;          ~index-sym (long 0)]
;     (if vals#
;       (let [~val-sym (first vals#)]
;             ~@code
;             (recur (next vals#) (inc ~index-sym)))
;       nil))))

(defn valid-shape?
  "returns true if the given object is a valid core.matrix array shape."
  ([shape]
    (try
      (and (>= (count shape) 0)
           (every? integer? shape))
      (catch #?(:clj Throwable :cljs js/Error) t false))))

(defn same-shape-object?
  "Returns true if two shapes are the same."
  ([sa sb]
    (cond
      (identical? sa sb) true
      (not= (count sa) (count sb)) false
      :else
        (let [ca (count sa)]
          (loop [i 0]
            (if (>= i ca)
              true
              (if (== (nth sa i) (nth sb i))
                (recur (inc i))
                false)))))))

(defn xor
  "Returns the logical xor of a set of values, considered as booleans"
  ([] false)
  ([x] (boolean x))
  ([x y] (if x (not y) (boolean y)))
  ([x y & more]
    (loop [p (xor x y) ss (seq more)]
      (if ss
        (recur (if (first ss) (not p) p) (next ss))
        p))))

(defn copy-double-array
  "Returns a copy of a double array"
  (^doubles [^doubles arr]
  #?(:clj (Arrays/copyOf arr (int (alength arr)))
     :cljs (.slice arr 0))))

(defn copy-long-array
  "Returns a copy of a long array"
  (^longs [^longs arr]
  #?(:clj (Arrays/copyOf arr (int (alength arr)))
     :cljs (.slice arr 0))))

(defn copy-object-array
  "Returns a copy of a long array"
  (^objects [^objects arr]
  #?(:clj (Arrays/copyOf arr (int (alength arr)))
     :cljs (.slice arr 0))))

(defn long-range
  "Returns a range of longs in a long[] array"
  ([end]
    (let [end (int end)
          ^longs arr (long-array end)]
      (dotimes [i end]
        (aset arr i (long i)))
      arr)))

#?(:clj
(defn to-long-array
  ([data]
    (if (is-long-array? data)
      data
      (long-array data))))
)

(defn long-array-of
  "Creates a long array with the specified values."
  ([] (long-array 0))
  ([a]
    (let [arr (long-array 1)]
      (aset arr 0 (long a))
      arr))
  ([a b]
    (let [arr (long-array 2)]
      (aset arr 0 (long a))
      (aset arr 1 (long b))
      arr))
  ([a b & more]
    (let [arr (long-array (+ 2 (count more)))]
      (aset arr 0 (long a))
      (aset arr 1 (long b))
      (doseq-indexed [x more i] (aset arr (+ 2 i) (long x)))
      arr)))

(defn object-array-of
  "Creates a long array with the specified values."
  ([] (object-array 0))
  ([a]
    (let [arr (object-array 1)]
      (aset arr 0 a)
      arr))
  ([a b]
    (let [arr (object-array 2)]
      (aset arr 0 a)
      (aset arr 1 b)
      arr))
  ([a b & more]
    (let [arr (object-array (+ 2 (count more)))]
      (aset arr 0 a)
      (aset arr 1 b)
      (doseq-indexed [x more i] (aset arr (+ 2 i) x))
      arr)))

(defn find-index
  "Returns the index of a value in a vector, or nil if not present" 
  ([^IPersistentVector v value]
   (let [n (#?(:clj .count :cljs count) v)]
      (loop [i 0]
        (when (< i n)
          (if (= value (#?(:clj .nth :cljs nth) v i))
            i
            (recur (inc i))))))))

(defn base-index-seq-for-shape
  "Returns a sequence of all possible index vectors for a given shape, in row-major order"
  [sh]
  (let [gen (fn gen [prefix rem]
              (if rem
                (let [nrem (next rem)]
                  (mapcat #(gen (conj prefix %) nrem) (range (first rem))))
                (list prefix)))]
    (gen [] (seq sh))))

(defn- broadcast-shape*
  "Returns the smallest shape that both shapes a and b can broadcast to, or nil if the the shapes
   are not compatible."
  ([a b]
    (cond
      (empty? a) (or b '())
      (empty? b) a
      (== 1 (first a)) (broadcast-shape* (first b) (next a) (next b))
      (== 1 (first b)) (broadcast-shape* (first a) (next a) (next b))
      (== (first a) (first b)) (broadcast-shape* (first a) (next a) (next b))
      :else nil))
  ([prefix a b]
    (if (or a b)
      (let [r (broadcast-shape* a b)]
        (if r (cons prefix r) nil))
      (cons prefix nil))))

(defn broadcast-shape
  "Returns the smallest compatible shape that a set of shapes can all broadcast to.
   Returns nil if this is not possible (i.e. the shapes are incompatible).
   Returns an empty list if both shape sequences are empty (i.e. represent scalars)"
  ([a] a)
  ([a b]
    (let [a (seq (reverse a))
          b (seq (reverse b))
          r (broadcast-shape* a b)]
      (if r (reverse r) nil))))

(defn can-broadcast
  "Returns truthy if the first shape a can be broadcast to the shape b"
  ([from-shape to-shape]
    (TODO)))

;; utilities for protocol introspection

#?(:clj
(defn extends-deep?
  "This functions differs from ordinary `extends?` by using `extends?`
   on all ancestors of given type instead of just type itself. It also
   skips `java.lang.Object` that serves as a default implementation
   for all protocols"
  [proto cls]
  ;; Here we need a special case to avoid reflecting on primitive type
  ;; (it will cause an exception)
  (if (= (Class/forName "[D") cls)
    (extends? proto cls)
    (let [bases (-> cls (r/type-reflect :ancestors true) :ancestors)]
      (->> bases
           (filter (complement #{'Object 'java.lang.Object}))
           (map resolve)
           (cons cls)
           (map (partial extends? proto))
           (some true?)))))
)

(defn protocol?
  "Returns true if an argument is a protocol'"
  [p]
  (and (map? p)
       (:on-interface p)
       (.isInterface ^Class (:on-interface p))))

(defn enhance-protocol-kv
  "Transform MapEntry to just map with some additional fields"
  [[name p]]
  (let [m (->> @p :var meta)]
    (assoc @p :line (:line m) :file (:file m) :name name)))

#?(:clj (do

(defn extract-protocols
  "Extracts protocol info from clojure.core.matrix.protocols"
  ([]
    (extract-protocols 'clojure.core.matrix.protocols))
  ([ns-sym]
    (->> (ns-publics ns-sym)
      (filter (comp protocol? deref val))
      (map enhance-protocol-kv)
      (sort-by :line))))

(defn unimplemented
  "Identifies which protocols are unimplemented for a given array object.

   Unimplemented protocols will fall back to the default implementation (for java.lang.Object) which
   is likely to be slower than a specialised implementation."
  [m]
  (let [protocols (extract-protocols)
        m (if (class? m) m (class m))]
    (map :name (filter #(not (#?(:clj extends-deep? :cljs satisfies?) % m)) protocols))))
))

(defn update-indexed [xs idxs f]
  (reduce #(assoc %1 %2 (f %2 (get %1 %2))) xs idxs))


