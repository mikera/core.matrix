(ns clojure.core.matrix.test-object-array
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.double-array))

(deftest instance-tests
  ;(clojure.core.matrix.compliance-tester/instance-test (object-array []))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [1]))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [1 :foo]))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [-1 4 2 7 -3])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (object-array [0.23])))
