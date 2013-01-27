(ns core.matrix.impl.dummy
  (:use core.matrix)
  (:use core.matrix.utils)
  (:use clojure.test)
  (:require [core.matrix.protocols :as mp])
  (:require [core.matrix.compliance-tester])
  (:require [core.matrix.implementations :as imp]))

(defrecord Dummy [dims])

(extend-protocol mp/PImplementation
  Dummy
    (implementation-key [m] 
      :dummy)
    (new-vector [m length] 
      (Dummy. [length]))
    (new-matrix [m rows columns] 
      (Dummy. [rows columns]))
    (new-matrix-nd [m dims] 
      (Dummy. (vec dims)))
    (construct-matrix [m data]
      (Dummy. (vec (shape data))))
    (supports-dimensionality? [m dims] 
      true))

(imp/register-implementation (Dummy. [1]))
