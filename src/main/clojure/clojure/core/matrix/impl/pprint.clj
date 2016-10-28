(ns clojure.core.matrix.impl.pprint
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [java.lang StringBuilder]
           [clojure.lang IPersistentVector]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^String NL (System/getProperty "line.separator"))

;; pretty-printing utilities for matrices

(defn- format-num [x] (format "%.3f" (double x)))

(defn- default-formatter [x]
  (cond 
    (integer? x) (.toString ^Object x)
    (number? x) (format-num x)
    :else (str x)))

(defn- column-lengths
  "Finds the longest string length of each column in an array of Strings."
  [m]
  (let [ss (mp/get-slice-seq m (dec (long (mp/dimensionality m))))]
    (mapv
      (fn [s] (mp/element-reduce s
                                 (fn [acc ^String e] (max (long acc) (.length e)))
                                 0))
      ss)))

(defn- format-array
  "Formats an array according to the given formatter function"
  ([m {:keys [prefix formatter column-names?] :as options}]
    (let [formatter (or formatter default-formatter)
          dims (long (mp/dimensionality m))
          m (if (or (== dims 0) (= false column-names?))
              m ;; explicity turned off column names
              (let [cnames (mp/column-names m)]
                (if (or column-names? cnames)
                  (vec (cons (or cnames 
                                 (vec (range (mp/dimension-count m (dec dims))))) 
                             (if (== dims 1)
                               [(mp/convert-to-nested-vectors m)] ;; 1D = single row
                               (mp/get-major-slice-seq m))))
                  m ;; no columns names present and not forced
                  )))
          m (mp/ensure-type m String)
          ]
      (cond
        (mp/is-scalar? m) (formatter m)
        :else (mp/element-map (mp/convert-to-nested-vectors m) formatter)))))

(defn- append-elem
  "Appends an element, right-padding up to a given column length."
  [^StringBuilder sb ^String elem ^long clen]
  (let [c (long (count elem))
        ws (- clen c)]
    (dotimes [i ws]
      (.append sb \space))
    (.append sb elem)))

(defn- append-row
  "Appends a row of data."
  [^StringBuilder sb row ^IPersistentVector clens] ;; the first element doesn't have a leading ws.
  (let [cc (.count clens)]
    (.append sb \[)
    (dotimes [i cc]
      (when (> i 0) (.append sb \space))
      (append-elem sb (mp/get-1d row i) (.nth clens i)))
    (.append sb \])))

(defn- rprint
  "Recursively joins each element with a leading
   line break and whitespace. If there are no
   elements left in the matrix it ends with a
   closing bracket."
  [^StringBuilder sb a pre clens]
  (let [dims (long (mp/dimensionality a))
        sc (long (mp/dimension-count a 0))
        pre2 (str pre " ")]
    (.append sb \[)
    (dotimes [i sc]
      (let [s (mp/get-major-slice a i)]
        (when (> i 0)
          (.append sb NL)
          (.append sb pre2))
        (if (== 2 dims)
          (append-row sb s clens)
          (rprint sb s pre2 clens))))
    (.append sb \])))

(defn pm
  "Pretty-prints an array. Returns a String containing the pretty-printed representation."
  ([a]
    (pm a nil))
  ([a {:keys [prefix formatter column-names?] :as options}]
    (let [m (format-array a options)
          prefix (or prefix "")
          sb (StringBuilder.)
          dims (long (mp/dimensionality m))]
      (.append sb prefix)
      (cond
        (== 0 dims) (.append sb (str (mp/get-0d m)))
        (== 1 dims)
          (append-row sb m (column-lengths m))
        :else
          (let [clens (column-lengths m)] 
            (rprint sb m prefix clens)))
      (.toString sb))))
