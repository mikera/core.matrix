(ns core.matrix.benchmark
  (:use core.matrix)
  (:require [criterium.core :as c])
  (:require [core.matrix.impl.persistent-vector]))

(defn benchmarks []
  
  (c/bench (add [[1 1]] [[1 1]]))
  
  
  ) 