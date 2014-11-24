(ns clojure.core.matrix.impl.ndarray-object
  (:refer-clojure :exclude [vector?])
  (:require [clojure.walk :as w]
            [clojure.core.matrix.impl.default]
            [clojure.core.matrix.impl.ndarray-magic :as magic]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.utils :refer :all]
            [clojure.core.matrix.impl.ndarray-macro :refer :all]
            [clojure.core.matrix.impl.ndarray :refer :all]))

(magic/spit-code :object)
