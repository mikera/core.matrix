(ns clojure.core.matrix.impl.dummy
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [clojure.core.matrix.implementations :as imp]))

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
