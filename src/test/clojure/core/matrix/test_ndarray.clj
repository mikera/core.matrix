(ns core.matrix.test-ndarray
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require core.matrix.impl.persistent-vector)
  (:require core.matrix.impl.ndarray))
