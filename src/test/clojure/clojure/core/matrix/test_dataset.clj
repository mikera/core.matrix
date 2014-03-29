(ns clojure.core.matrix.test-dataset
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.dataset :as ds])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.double-array))


(deftest regressions
  (is (= [2] (seq (emap inc (object-array [1]))))))

(deftest instance-tests
  ;(clojure.core.matrix.compliance-tester/instance-test (object-array []))
  (clojure.core.matrix.compliance-tester/instance-test (ds/dataset [:bar] [["Foo"]])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test ds/CANONICAL-OBJECT))
