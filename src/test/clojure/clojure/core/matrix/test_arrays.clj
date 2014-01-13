(ns clojure.core.matrix.test-arrays
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:refer-clojure :exclude [vector?])
  (:use clojure.test))

;; This namespace is intended for tests of core.matrix functions on arbitrary Java arrays
;;
;; should use ints, floats etc. where possible (since doubles and Objects are tested elsewhere)

(deftest int-array-test
  (let [a (int-array [1 2 3])]
    (is (== 1 (dimensionality a)))
    (is (vec? a))
    (is (== 6 (esum a)))
    (is (equals (pow a a) [1 4 27]))))

(deftest array-slicing
  (let [a (long-array [1 2 3])]
    (is (== 1 (first (slices a)))))
  (let [a (object-array [1 2 3])]
    (is (array? (first (slice-views a)))))) 

(deftest compliance-tests
  (clojure.core.matrix.compliance-tester/instance-test (int-array [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (float-array [1 2 3]))
  (clojure.core.matrix.compliance-tester/instance-test (long-array []))
  (clojure.core.matrix.compliance-tester/instance-test (char-array [\a \b \c]))
  (clojure.core.matrix.compliance-tester/instance-test (object-array [(double-array [1 2 3])]))
  ;; TODO: reinstante once Clojure bug CLJ-1306 is fixed
  ;; (clojure.core.matrix.compliance-tester/instance-test (object-array [(short-array (map short [1 2 3]))]))
  (clojure.core.matrix.compliance-tester/instance-test 
    (into-array (Class/forName "[D") 
                [(double-array [1 2])
                 (double-array [3 4])]))
  )