(ns clojure.core.matrix.impl.pprint
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils))

;; pretty-printing utilities for matrices

(defn- format-num [x] (format "%.3f" (double x)))

(defn- longest-nums
  "Finds the longest string representation of
   a number in each column within a given matrix."
  [mat]
  (let [tmat (mp/transpose mat)
        col-long (fn [r] (mp/element-reduce (mp/element-map r #(count (format-num %))) max))]
    (map col-long (mp/get-major-slice-seq tmat))))

(defn- str-elem
  "Prints and element that takes up a given amount of whitespaces."
  [elem whitespaces]
  (let [formatter (str "%" whitespaces "." 3 "f")]
    (format formatter (double elem))))

(defn- str-row
  "Creates a string for each row with the desired
   amount of spaces between the elements."
  [[elem-head & elem-tail] [len-head & len-tail]] ;; the first element doesn't have a leading ws.
  (let [first-elem (str-elem elem-head len-head)
        body-elems (map str-elem elem-tail len-tail)]
  (str "[" first-elem (apply str (map #(str " " %) body-elems)) "]")))

(defn- rprint
  "Recursively prints each element with a leading
   line break and whitespace. If there are no
   elements left in the matrix it ends with a
   closing bracket."
  [[head & tail :as mat] acc len]
  (if (empty? mat)
    (str acc "]")
    (recur tail (str acc "\n " (str-row head len)) len)))

(defn pm
  "Pretty-prints a matrix"
  [m]
  (cond
    (mp/is-scalar? m) (println (format-num m))
    (== 1 (mp/dimensionality m)) (println (str-row m (longest-nums m)))
    :else 
      (let [len (longest-nums m)
            rows (mp/get-major-slice-seq m) 
            start (str "[" (str-row (first rows) len))
            out (str start (rprint (next rows) "" len))]
         (println out))))