(ns clojure.core.matrix.test-arrays
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:refer-clojure :exclude [vector?])
  (:use clojure.test))

(deftest int-array-test
  (let [a (int-array [1 2 3])]
    (is (== 1 (dimensionality a)))
    (is (vec? a))
    (is (== 6 (esum a)))))

(deftest compliance-tests
  (clojure.core.matrix.compliance-tester/instance-test (int-array [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (float-array [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (char-array [\a \b \c])))