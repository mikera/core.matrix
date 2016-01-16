(ns clojure.core.matrix.impl.double-array
  "Implementation supporting:

   - Javascript double[] arrays as core.matrix 1D vectors
   - Javascript double[][] arrays as core.matrix 2D matrices"
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.utils :as u])
  (:require-macros [clojure.core.matrix.macros :refer [error is-double-array?]]))

(defn new-double-array
  [[len & shape]]
  (let [ary (double-array len)]
    (when (not (empty? shape))
      (doseq [i (range (count shape))]
        (aset ary i (new-double-array shape))))
    ary))
