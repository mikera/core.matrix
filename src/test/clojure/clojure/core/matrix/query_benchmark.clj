(ns clojure.core.matrix.query-benchmark
  (:require [criterium.core :as c])
  (:use clojure.core.matrix) 
  (:require [clojure.core.matrix.protocols :as mp]) 
  (:import [clojure.core.matrix ClassPair]))

;; benchmark for querying array properties

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def pv [1])

(defn all-benchmarks []

(c/quick-bench (dotimes [i 1000] 
                 (dimensionality pv)))
;; 8288 ns!?!

(c/quick-bench (dotimes [i 1000] 
                 (mp/dimensionality pv)))
;; 8497 ns???

(c/quick-bench (dotimes [i 1000] 
                 (array? pv)))
;; 54,518 ns ?????


)
