(ns clojure.core.matrix.impl.pprint
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils))

;; pretty-printing utilities for matrices

(defn- format-num [x] (format "%.3f" (double x)))

(defn- default-formatter [x]
  (if (number? x)
    (format-num x)
    (str x)))

(defn- longest-nums
  "Finds the longest string representation of
   any element in each column within a given matrix."
  [mat formatter]
  (let [tmat (mp/transpose mat)
        col-long (fn [r] (mp/element-reduce (mp/element-map r #(count (formatter %))) max))]
    (map col-long (mp/get-major-slice-seq tmat))))

(defn- str-elem
  "Prints an element that takes up a given amount of whitespaces."
  [elem whitespaces]
  (let [formatter (str "%" whitespaces "." 3 "f")]
    (format formatter (double elem))))

(defn- str-row
  "Creates a string for each row with the desired
   amount of spaces between the elements."
  [row len] ;; the first element doesn't have a leading ws.
  (let [[elem-head & elem-tail] (mp/get-major-slice-seq row)
        [len-head & len-tail] len
        first-elem (str-elem elem-head len-head)
        body-elems (map str-elem elem-tail len-tail)]
  (str "[" first-elem (apply str (map #(str " " %) body-elems)) "]")))

(defn- rprint
  "Recursively joins each element with a leading
   line break and whitespace. If there are no
   elements left in the matrix it ends with a
   closing bracket."
  [[head & tail :as mat] acc len]
  (cond 
    (empty? mat)
      (str acc "]")
    (== 1 (mp/dimensionality head))
      (recur tail (str acc "\n " (str-row head len)) len)
    :else 
      (error "Printing of higher dimensional arrays not yet supported")))

(defn pm
  "Pretty-prints a matrix"
  [m & {:keys [prefix formatter]}]
  (let [formatter (or formatter default-formatter)]
    (cond
     (mp/is-scalar? m) (str prefix (formatter m))
     (== 1 (mp/dimensionality m)) (str prefix (str-row m (longest-nums m formatter)))
    :else 
       (let [len (longest-nums m formatter)
             rows (mp/get-major-slice-seq m) 
             start (str prefix "[" (str-row (first rows) len))
             out (str start (rprint (next rows) prefix len))]
         out))))