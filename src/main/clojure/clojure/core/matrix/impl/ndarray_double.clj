(ns clojure.core.matrix.impl.ndarray-double
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.impl.default]
            [clojure.core.matrix.impl.ndarray-magic :as magic]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.impl.mathsops :as mops]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.walk :as w]
            [clojure.core.matrix.impl.ndarray :refer :all]
            [clojure.core.matrix.utils :refer :all]
            [clojure.core.matrix.impl.ndarray-macro :refer :all]))

(magic/spit-code :double)
