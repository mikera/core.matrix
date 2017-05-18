(ns clojure.core.matrix.impl.common
  "Namespace containing common functions useful for core.matrix implementatations"
  (:require [clojure.string :as s]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :as u]
            [clojure.core.matrix.implementations :as mi]
    #?(:clj [clojure.java.shell :refer [sh]]))
  (#?(:clj :require :cljs :require-macros)
           [clojure.core.matrix.macros :refer [scalar-coerce]]))

(defn get-impl-objs
  "Returns a list of available implementations' objects"
  []
  (filter second
          (for [[name ns] mi/KNOWN-IMPLEMENTATIONS
                :when (not (#{:TODO :persistent-vector} ns))]
            (try
              {:name name, :obj (mi/get-canonical-object name)}
              (catch #?(:clj Throwable :cljs js/Error) t nil)))))

#?(:clj (do

(defn find-implementers
  "Returns a set of implementation names of implementations that
   support provided protocol"
  [protocol impl-objs]
  (->> impl-objs
       (filter #(->> % :obj class (u/extends-deep? protocol)))
       (map :name)
       (into #{})))

(defn extract-implementations
  "Returns a a sequence of protocol maps augmented with :implemented-by key
   that contains a set of names of supporting implementations"
  [protocols impl-objs]
  (for [proto protocols]
    (assoc proto :implemented-by (find-implementers proto impl-objs))))

(defn get-git-hash
  "Returns current revision's git hash"
  []
  (-> (sh "git" "log" "--pretty=format:'%H'" "-n 1")
      :out
      (s/replace #"'" "")))

))

(defn construct-matrix
  "Constructs an array from the provided data, attempting to use the given implementation.
   Uses a default implementation if needed"
  ([impl data]
    (or (mp/construct-matrix impl data)
        (try 
          (mp/construct-matrix mi/*matrix-implementation* data)
          (catch ClassCastException t nil)) ;; fix for element type not handled
        (mp/construct-matrix [] data))))

(defn mapmatrix
  "Maps a function over all components of a persistent vector matrix. Like mapv but for matrices.
   Assumes correct dimensionality / shape.

   First array argument must be nested persistent vectors. Others may be
   any arrays of the same shape.

   Returns a nested persistent vector matrix or a scalar value."
  ([f m]
   (let [dims (long (mp/dimensionality m))]
     (cond
       (== 0 dims) (f (scalar-coerce m))
       (== 1 dims) (construct-matrix m
                     (map f (mp/element-seq m)))
       :else
       (let [res (map (partial mapmatrix f) (mp/get-major-slice-seq m))]
         (mp/reshape (mp/coerce-param m res)
                     (vec (concat [(first (mp/get-shape m))] (next (mp/get-shape res)))))))))
  ([f m1 m2]
    (let [dims (long (mp/dimensionality m1))]
      (cond
        (== 0 dims) (f (scalar-coerce m1) (scalar-coerce m2))
        (== 1 dims) (construct-matrix m1
                      (map f (mp/element-seq m1) (mp/element-seq m2)))
        :else
        (let [res (map (partial mapmatrix f) (mp/get-major-slice-seq m1) (mp/get-major-slice-seq m2))]
          (mp/reshape (mp/coerce-param m1 res)
            (vec (concat [(first (mp/get-shape m1))] (next (mp/get-shape res)))))))))
  ([f m1 m2 m3]
    (let [dims (long (mp/dimensionality m1))]
      (cond
        (== 0 dims) (f (scalar-coerce m1) (scalar-coerce m2) (scalar-coerce m3))
        (== 1 dims) (construct-matrix m1
                      (map f (mp/element-seq m1) (mp/element-seq m2) (mp/element-seq m3)))
        :else
        (let [res (mapv (partial mapmatrix f)
                        (mp/get-major-slice-seq m1)
                        (mp/get-major-slice-seq m2)
                        (mp/get-major-slice-seq m3))]
          (mp/reshape (mp/coerce-param m1 res)
            (vec (concat [(first (mp/get-shape m1))] (next (mp/get-shape res)))))))))
  ([f m1 m2 m3 & more]
    (let [dims (long (mp/dimensionality m1))]
      (cond
        (== 0 dims) (apply f (scalar-coerce m1) (scalar-coerce m2)
                           (scalar-coerce m3) (map mp/get-0d more))
        (== 1 dims) (construct-matrix m1
                      (apply map f (mp/element-seq m1) (mp/element-seq m2)
                             (mp/element-seq m3) (map mp/element-seq more)))
        :else
        (let [res (apply map (partial mapmatrix f)
                         (mp/get-major-slice-seq m1)
                         (mp/get-major-slice-seq m2)
                         (mp/get-major-slice-seq m3)
                         (map mp/get-major-slice-seq more))]
          (mp/reshape (mp/coerce-param m1 res)
            (vec (concat [(first (mp/get-shape m1))] (next (mp/get-shape res))))))))))

(defn logistic-fn
  "Logistic function, with primitive type hints"
  (^double [^double t]
    (let [e-t (Math/exp (- t))]
      (/ 1.0 (+ 1.0 e-t)))))

(defn softplus-fn
  "Softplus function, with primitive type hints"
  (^double [^double t]
    (if (> t 100.0) ;; catch the case of overflow to infinity for large inputs
      t
      (let [et (Math/exp t)]
        (Math/log (+ 1.0 et))))))

(defn relu-fn
  "ReLU function, with primitive type hints"
  (^double [^double t]
    (Math/max 0.0 t)))

(defn square?
  "Returns true if matrix is square (2D with same number of rows and columns)"
  ([m]
    (and
      (== 2 (long (mp/dimensionality m)))
      (== (long (mp/dimension-count m 0)) (long (mp/dimension-count m 1))))))

;; Helper function for symmetric? predicate in PMatrixPredicates.
;; Note loop/recur instead of letfn/recur is 20-25% slower.
;; not possible to eliminate boxing warnings - needs to handle any numeric type
(defn symmetric-matrix-entries?
  "Returns true iff square matrix m is symmetric."
  [m]
  (let [dim (long (first (mp/get-shape m)))]
    (letfn [(f [^long i ^long j]
              (cond
                (>= i dim) true                         ; all entries match: symmetric
                (>= j dim) (recur (+ 1 i) (+ 2 i))      ; all j's OK: restart with new i
                (= (mp/get-2d m i j)
                   (mp/get-2d m j i)) (recur i (inc j)) ; OK, so check next pair
                :else false))]                          ; not same, not symmetric
      (f 0 1))))

