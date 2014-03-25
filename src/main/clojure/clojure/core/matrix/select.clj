(ns clojure.core.matrix.select
  (:use [clojure.core.matrix])
  (:require [clojure.core.matrix.protocols :as mp]))

;; =========================================================
;; EXPERIMENTAL - API subject to change
;;
;; high-level matlab-like indexing

(defn- eval-arg [a d arg]
  (cond
   (sequential? arg) arg
   (number? arg) [arg]
   (keyword? arg) arg
   :else (eval-arg a d (arg a d))))

(defn- eval-args [a args]
  (map (partial eval-arg a) (range) args))

(defn sel
  "matlab-like array indexing.
   Examples:
    (sel [[1 2][3 4]] 0 0) ;=> 1
    (sel [[1 2][3 4]] [0 1] 0) ;=> [[1] [3]] (gets the first column)
    :* is shorthand for select all
    (sel [[1 2][3 4]] :* :*) ;=> [[1 2][3 4]]
    sel also supports selector functions:
    (sel [[1 2][3 4]] (irange) (irange));=> [[1 2][3 4]]
    (sel [[1 2][3 4]] end end) ;=> 4
    (sel [[1 2][3 4]] (exclude 1) (exclude 0)) ;=> 2
    sel supports logical indexing
    (sel [[-1 0][1 2]] (where pos?)) ;=> [1 2]"
  [a & args]
  (if (and (= 1 (count args)) (< 1 (dimensionality a)))
    (mp/linear-sel a (eval-arg (mp/linear-view a) 0 (first args)))
    (mp/area-sel a (eval-args a args))))

(defn- higher-order-sel-set [a args linear-set area-set]
  (let [vals (last args) args (butlast args)]
    (if (and (= 1 (count args)) (< 1 (dimensionality a)))
      (linear-set a (eval-arg (mp/linear-view a) 0 (first args)) vals)
      (area-set a (eval-args a args) vals))))

(defn sel-set
  "like sel, but sets the selected indices to the values specified in the
   last argument. Leaves first argument unchanged. Examples
   (sel-set [[1 2][3 4]] 0 0 2) ;=> [[2 2][3 4]]
   (sel-set [[1 2][3 4]] :* 0 [[5][6]] ;=> [[5 2][6 4]]
   (sel-set [[1 2][3 4]] :* :* 1) ;=> [[1 1][1 1]]
   (sel-set [[-2 -1][0 1]] (where neg?) 0) ;=> [[0 0][0 1]]"
  [a & args]
  (higher-order-sel-set a args mp/linear-set mp/area-set))

(defn sel-set!
  "like sel-set but modifies argument in place"
  [a & args]
  (higher-order-sel-set a args mp/linear-set! mp/area-set!))

(defn end
  "selector function for sel. selects the last alid index"
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