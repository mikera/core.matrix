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

(defn- expand-* [a args]
  [(map #(if (= :* %1) (mp/dimension-count a %2) (count %1)) args (range))
   (map #(if (= :* %1) (range (mp/dimension-count a %2)) %1) args (range))])

(defn sel-area [a args]
  (let [[shape args] (expand-* a args)]
    (cond
     (= shape (clojure.core.matrix/shape a)) a
     (and (= (count shape) 2)
          (= (first shape) (mp/dimension-count a 0)))
     (if (= (second shape) 1)
       (mp/column-matrix a (get-column a (first (second args))))
       (mp/transpose (matrix a (map (partial get-column a) (second args)))))
     (and (= (count shape) 2)
          (= (second shape) (mp/dimension-count a 1)))
     (if (= (first shape) 1)
       (mp/get-row a (first (first args)))
       (matrix (map (partial get-row a) (first args))))
     :else
     (compute-matrix
      a shape (fn [& idx]
                (apply (partial mget a) (map #(nth %1 %2) args idx)))))))


(defn normal-sel [a args]
  (let [area (eval-args a args)
        erg(sel-area a area)]
    (if (= 1 (mp/element-count erg)) (first (mp/element-seq erg)) erg)))



(defn get-indices [a indices]
  (array a (map #(apply (partial mget a) %1) indices)))

(defn linear-view [a]
  (mp/element-seq a))

(defn int-to-index [shape int]
  (let [weights (map #(reduce * %) (take-while seq (iterate rest (rest shape))))]
    (loop [ind [] r int [w & ws] weights]
      (if w
        (recur (conj ind (quot r w)) (rem r w) ws)
        (conj ind r)))))

(defn get-linear-indices [a arg]
  (map #(int-to-index (shape a) %) (eval-arg (linear-view a) 0 arg)))

(defn linear-sel [a arg]
  (get-indices a (get-linear-indices a arg)))

(defn sel
  "matlab-like array indexing.
   Examples:
    (sel [[1 2][3 4]] 0 0) ;=> 1
    (sel [[1 2][3 4]] [0 1] 0) ;=> [[1] [3]] (gets the first column)
    sel also supports extractors:
    (sel [[1 2][3 4]] (irange) (irange));=> [[1 2][3 4]]
    (sel [[1 2][3 4]] end end) ;=> 4
    (sel [[1 2][3 4]] (exclude 1) (exclude 0)) ;=> 2
    sel supports logical indexing
    (sel [[-1 0][1 2]] (where pos?)) ;=> [1 2]"
  [a & args]
  (if (and (= 1 (count args)) (< 1 (dimensionality a)))
    (linear-sel a (first args))
    (normal-sel a args)))

(defn set-indices [a indices values]
  (let [values (eseq (broadcast values (shape indices)))]
    (loop [a a [id & idx] indices [v & vs] values]
      (if id
        (recur (apply (partial mset a) (concat id [v])) idx vs)
        a))))

(defn set-indices! [a indices values]
  (let [values (eseq (broadcast values (shape indices)))]
    (loop [[id & idx] indices [v & vs] values]
      (when id
        (do (apply (partial mset! a) (concat id [v])) (recur idx vs))))))

(defn linear-sel-set [a arg values]
  (set-indices a (get-linear-indices a arg) values))

(defn linear-sel-set! [a arg values]
  (set-indices! a (get-linear-indices a arg) values))

(defn- area-indices [area]
  (reduce (fn [io in]
            (for [a in b io]
              (cons a b))) (map vector (last area)) (rest (reverse area))))

(defn indices [vals]
  (area-indices (map range (shape vals))))

(defn- higher-order-set-area [a area vals set-whole
                              set-columns set-rows set-general]
  (let [[shape area] (expand-* a area)
        vals (broadcast vals shape)]
    (cond
     (= shape (clojure.core.matrix/shape a)) (set-whole a area shape vals)
     (and (= (count shape) 2)
          (= (first shape) (mp/dimension-count a 0)))
     (set-columns a area shape vals)
     (and (= (count shape) 2)
          (= (second shape) (mp/dimension-count a 1)))
     (set-rows a area shape vals)
     :else
     (set-general a area shape vals))))

(defn set-area [a area vals]
  (higher-order-set-area
   a area vals
   (fn [a area shape vals] vals)
   (fn [a area shape vals]
     (loop [a a [i & is] (second area) [j & js] (range (second shape))]
       (if i (recur (set-column a i (get-column vals j)) is js) a)))
   (fn [a area shape vals]
     (loop [a a [i & is] (first area) [j & js] (range (first shape))]
       (if i (recur (set-row a i (get-row vals j)) is js) a)))
   (fn [a area shape vals]
     (loop [a a [idl & idxl] (area-indices area) [idr & idxr] (indices vals)]
       (if idl (recur (apply (partial mset a)
                             (concat idl [(apply (partial mget vals) idr)]))
                      idxl idxr) a)))))

(defn set-column! [a i col]
  (loop [[j & js] (range (row-count a)) [c & cs] col]
    (when j (mset! a j i c) (recur js cs))))

(defn set-area! [a area vals]
  (higher-order-set-area
   a area vals
   (fn [a area shape vals]
     (loop [[i & is] (indices a) [j & js] (indices vals)]
       (when i
         (apply (partial mset! a) (concat i [(apply (partial mget vals) j)]))
         (recur is js))))
   (fn [a area shape vals]
     (loop [[i & is] (second area) [j & js] (range (second shape))]
       (when i (do (set-column! a i (get-column vals j)) (recur is js)))))
   (fn [a area shape vals]
     (loop [[i & is] (first area) [j & js] (range (first shape))]
       (when i (do (set-row! a i (get-row vals j)) (recur is js)))))
   (fn [a area shape vals]
     (loop [[idl & idxl] (area-indices area) [idr & idxr] (indices vals)]
       (when idl (do (partial mset! a)
                     (concat idl [(apply (partial mget vals) idr)])
                     (recur idxl idxr)))))))



(defn normal-sel-set [a args vals]
  (set-area a (eval-args a args) vals))

(defn normal-sel-set! [a args vals]
  (set-area! a (eval-args a args) vals))

(defn- higher-order-sel-set [a args linear-set normal-set]
  (let [vals (last args) args (butlast args)]
    (if (and (= 1 (count args)) (< 1 (dimensionality a)))
      (linear-set a (first args) vals)
      (normal-set a args vals))))

(defn sel-set [a & args]
  (higher-order-sel-set a args linear-sel-set normal-sel-set))

(defn sel-set! [a & args]
  (higher-order-sel-set a args linear-sel-set! normal-sel-set!))

(defn end
  "extractor for sel. selects the last alid index"
  [a dim]
  (- (dimension-count a dim) 1))

(defn irange
  "extractor for sel.
   index-range selects the range from start position until (including!) the end
   position. Also supports extractors as arguments
   Example: (sel [0 1 2 3 4] (irange 1 end)) ;=> [1 2 3 4]
   (irange) is the same as (irange 0 end)"
  ([] (irange 0 end 1))
  ([end] (irange 0 end 1))
  ([start end] (irange 0 end 1))
  ([start end step]
     (fn [a dim]
       (let [[start end step] (map #(if (number? %) % (% a dim))
                                   [start end step])]
         (range start (if (pos? step) (inc end) (dec end)) step)))))

(defn exclude
  "extractor for sel.
   selects all valid indizes except the ones specified in idx. idx can be a
   number or a sequential"
  [idx]
  (fn [a dim]
    (let [count (dimension-count a dim)]
      (remove (set (eval-arg a dim idx)) (range count)))))

(defn where
  "extractor for sel.
   Enables logical indexing. Selects all indices where pred? succeeds.
   Can only be used as second argument to sel. example:
   (sel (range 10) (where (partial > 5))) ;=> [0 1 2 3 4]"
  [pred?]
  (fn [a dim]
    (remove nil? (map (fn [elem idx]
                        (when (pred? elem) idx))
                      (mp/element-seq a) (range)))))

(defn even
  "extractor for sel.
   selects all valid even indices"
  [a dim]
  (let [c (dimension-count a dim)]
    (range 0 c 2)))

(defn odd
  "extractor for sel.
   selects all valid odd indices"
  [a dim]
  (let [c (dimension-count a dim)]
    (range 1 c 2)))