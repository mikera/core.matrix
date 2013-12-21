(ns clojure.core.matrix.test-vectorz
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [mikera.vectorz.matrix-api])
  (:use clojure.test))

(deftest compliance-tests
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[1 2] [3 4]]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]])))