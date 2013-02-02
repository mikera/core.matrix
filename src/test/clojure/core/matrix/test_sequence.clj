(ns core.matrix.test-sequence
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require [core.matrix.compliance-tester])
  (:require core.matrix.impl.sequence))


(deftest test-sequence-shape
  (is (= [] (shape 1)))
  (is (= [2] (shape '(1 2))))
  (is (= [2 2] (shape '((1 2) (3 4))))))

(deftest compliance-test
  (core.matrix.compliance-tester/compliance-test '(1)))
