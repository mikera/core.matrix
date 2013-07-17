(ns clojure.core.matrix.docgen.bench
  (:require [criterium.core :as cr]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.docgen.common :as c]))

(def conf {:tobench [:vectorz :ndarray]
           :reference :vectorz
           :fast-bench true})

(defn bench-proto [proto]
  ()
  )

(defn generate []
  (let [protos (c/extract-protocols)]

    ))