(ns clojure.core.matrix.selection
  "Namespace for fully featured core.matrix select API.

   Supports selection functions, which are defined as:
      (fn [array dim] ...) => set of indices for dimension "
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :refer [select set-selection set-selection! slice dimensionality dimension-count]])
  (#?(:clj :require :cljs :require-macros)
           [clojure.core.matrix.macros :refer [TODO error]]))

;; high-level array indexing
;; Provides a matlab-like dsl for matrix indexing.
;; Builds upon the select functions in clojure.core.matrix and
;; adds support for linear-indexing, logical-indexing and use of selector
;; functions to build up selections.

(defn- eval-args
  "Runs through a set of args and evaluates them against the array and current dimension if it is a function."
  [a args]
  (when (not= (count args) (dimensionality a))
    (error "Attempting to select with wrong number of dimensions with args: " args))
  (vec (map-indexed
         (fn [i arg]
           (cond
             (vector? arg) arg
             (fn? arg) (arg a i)
             :else arg))
         args)))

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
  (let [sel-args (eval-args a args)]
    (apply select a sel-args)))

(defn set-sel
  "like sel but sets the values of a at the selected indices to the supplied
   values. Leaves a unchanged, returns the modified array. Examples:
   (sel-set [[1 2][3 4]] 0 0 2) ;=> [[2 2][3 4]]
   (sel-set [[1 2][3 4]] (irange) 0 [[5][6]] ;=> [[5 2][6 4]]
   (sel-set [[1 2][3 4]] (irange) (irange) 1) ;=> [[1 1][1 1]]
   (sel-set [[-2 -1][0 1]] (where neg?) 0) ;=> [[0 0][0 1]]"
  [a & args]
  (let [val (last args)
        sel-args (eval-args a (butlast args))]
    (apply set-selection a (concat sel-args  (list val)))))

(defn set-sel!
  "like set-sel but destructively modifies a in place"
  [a & args]
  (let [val (last args)
        sel-args (eval-args a (butlast args))]
    (apply set-selection! a (concat sel-args  (list val)))))

;;Selector functions

(defn end
  "selector function for sel. selects the last valid index"
  [a dim]
  (dec (dimension-count a dim)))

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
   selects all valid indices except the ones specified in idx. idx can be a
   number, a set or a sequence of indices"
  [idx]
  (fn [a dim]
    (let [idx (if (number? idx) #{idx} (set idx))
          count (dimension-count a dim)
          excl (remove idx (range count))]
      (if (second excl) excl (first excl)))))

(defn where
  "selector function for sel.
   Enables logical indexing. Selects all indices where pred? succeeds.
   Can only be used as second argument to sel. example:
   (sel (range 10) (where (partial > 5))) ;=> [0 1 2 3 4]"
  [pred?]
  (fn [a dim]
    (let [n (dimension-count a dim)]
      (filterv pred? (range n)))))

(defn where-slice
  "selector function for sel.
   Selects all indices where pred? returns true when called against the respecitive slice on the given dimension
   Can only be used as second argument to sel. example:
   (sel [1 2 3 -1 -1 4 5 6] (where-slice pos?)) ;=> [1 2 3 4 5 6]"
  [pred?]
  (fn [a dim]
    (let [n (dimension-count a dim)]
      (filterv (fn [i] (pred? (slice a dim i))) (range n)))))

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
