(ns clojure.core.matrix.select
  (:use [clojure.core.matrix])
  (:require [clojure.core.matrix.protocols :as mp]))

;; high-level array indexing
;; Provides a matlab-like dsl for matrix indexing.
;; Builds upon the select functions in clojure.core.matrix and
;; adds support for linear-indexing, logical-indexing and use of selector
;; functions to build up selections.

(defn- eval-arg
  "desugars the selector"
  [a d arg]
  (cond
   (sequential? arg) (mp/element-seq arg)
   (number? arg) [arg]
   (= :all arg) (range (dimension-count a d))
   :else (eval-arg a d (arg a d))))

(defn- eval-args
  "evaluates the arguments - calls the selector functions"
  [a args]
  (map (partial eval-arg a) (range) args))

(defn- reduce-dims
  "Strips leading dimensions with dimension-count 1"
  [erg]
  (let [shape (mp/get-shape erg)]
    (reduce (fn [acc s]
              (if (= s 1)
                (first (mp/get-major-slice-seq acc))
                (reduced acc))) erg shape)))

(defn- int-to-index
  "gets the index in an array of shape from the position in the element-seq"
  [shape int]
  (let [weights (map #(reduce * %) (take-while seq (iterate rest (rest shape))))]
    (loop [ind [] r int [w & ws] weights]
      (if w
        (recur (conj ind (quot r w)) (rem r w) ws)
        (conj ind r)))))

(defn- get-linear-indices
  "returns the corresponding indices for arg"
  [a arg]
  (map #(int-to-index (mp/get-shape a) %) arg))

(defn sel
  "matlab-like array indexing.
   Like clojure.core.matrix/select but also supports selector functions:
    (sel [[1 2][3 4]] (irange) (irange));=> [[1 2][3 4]]
    (sel [[1 2][3 4]] end end) ;=> 4
    (sel [[1 2][3 4]] (exclude 1) (exclude 0)) ;=> 2
    if only one argument is supplied it does linear indexing - selects the
    elements by their position in eseq.
    (sel [[1 2][3 4]] [0 3]) ;=> [1 4]
    sel supports logical indexing:
    (sel [[-1 0][1 2]] (where pos?)) ;=> [1 2]"
  [a & args]
  (if (and (= 1 (count args)) (< 1 (dimensionality a)))
    (mp/get-indices a (get-linear-indices
                       a (eval-arg (mp/as-vector a) 0 (first args))))
    (reduce-dims (mp/select a (eval-args a args)))))

(defn set-sel
  "like sel but sets the values of a at the selected indices to the supplied
   values. Leaves a unchanged, returns the modified array. Examples:
   (sel-set [[1 2][3 4]] 0 0 2) ;=> [[2 2][3 4]]
   (sel-set [[1 2][3 4]] (irange) 0 [[5][6]] ;=> [[5 2][6 4]]
   (sel-set [[1 2][3 4]] (irange) (irange) 1) ;=> [[1 1][1 1]]
   (sel-set [[-2 -1][0 1]] (where neg?) 0) ;=> [[0 0][0 1]]"
  [a & args]
  (let [vals (last args) sel-args (butlast args)]
    (if (and (= 1 (count sel-args)) (< 1 (dimensionality a)))
      (mp/set-indices a (get-linear-indices
                         a (eval-arg (mp/as-vector a) 0 (first sel-args))) vals)
      (mp/set-selection a (eval-args a sel-args) vals))))

(defn set-sel!
  "like set-sel but destructively modifies a in place"
  [a & args]
  (let [vals (last args) sel-args (butlast args)]
    (if (and (= 1 (count sel-args)) (< 1 (dimensionality a)))
      (mp/set-indices! a (get-linear-indices
                          a (eval-arg (mp/as-vector a) 0 (first sel-args))) vals)
      (mp/assign! (mp/select a (eval-args a sel-args)) vals))))

;;Selector functions

(defn end
  "selector function for sel. selects the last valid index"
  [a dim]
  (- (dimension-count a dim) 1))

(defn calc [f & args]
  (fn [a dim]
    (apply f (map #(if (number? %) % (% a dim)) args))))

(defn irange
  "selector function for sel.
   index-range selects the range from start position until (including!) the end
   position. Also supports selector functions as arguments
   Example: (sel [0 1 2 3 4] (irange 1 end)) ;=> [1 2 3 4]
   (irange) is the same as (irange 0 end)"
  ([] (irange 0 end 1))
  ([end] (irange 0 end 1))
  ([start end] (irange start end 1))
  ([start end step]
     (fn [a dim]
       (let [[start end step] (map #(if (number? %) % (% a dim))
                                   [start end step])]
         (range start (if (pos? step) (inc end) (dec end)) step)))))

(defn exclude
  "selector function for sel.
   selects all valid indizes except the ones specified in idx. idx can be a
   number or a sequential"
  [idx]
  (fn [a dim]
    (let [count (dimension-count a dim)]
      (remove (set (eval-arg a dim idx)) (range count)))))

(defn where
  "selector function for sel.
   Enables logical indexing. Selects all indices where pred? succeeds.
   Can only be used as second argument to sel. example:
   (sel (range 10) (where (partial > 5))) ;=> [0 1 2 3 4]"
  [pred?]
  (fn [a dim]
    (remove nil? (map (fn [elem idx]
                        (when (pred? elem) idx))
                      (mp/element-seq a) (range)))))

(defn even
  "selector function for sel.
   selects all valid even indices"
  [a dim]
  (let [c (dimension-count a dim)]
    (range 0 c 2)))

(defn odd
  "selector function for sel.
   selects all valid odd indices"
  [a dim]
  (let [c (dimension-count a dim)]
    (range 1 c 2)))