(ns clojure.core.matrix.test-vectorz
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [mikera.vectorz.matrix-api])
  (:use clojure.test))

;; TODO: reinstate after vector-clj 0.18.0 release which contains fix
;;(deftest test-pm
;;  (is (string? (clojure.core.matrix.impl.pprint/pm (array :vectorz [1 2]))))) 

(deftest compliance-tests
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[1 2] [3 4]]))
  (clojure.core.matrix.compliance-tester/instance-test (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]])))