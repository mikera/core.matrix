(ns clojure.core.matrix.test-sequence
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.sequence))

;; Tests for core.matrix implementation for arbitray sequences (ISeq)

(deftest regressions
  (is (== 3 (ereduce (fn [acc _] (inc acc)) 0 '(nil nil nil))))
  (is (e== [-1 -2] (negate '(1 2))))
  (is (= '() (mget '()))))

(deftest test-sequence-shape
  (is (= [2] (shape '(1 2))))
  (is (= [2 2] (shape '((1 2) (3 4))))))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test '(1)))

(deftest sequence-ops
  (is (equals (emul (range 10) (range 10)) '(0 1 4 9 16 25 36 49 64 81))))

;; sequences should get converted to a better implementation
(deftest test-to-vectors
  (is (vector? (emap inc '(1 2 3)))))

(deftest test-empty-sequence
  (let [v '()]
    (is (== 0 (ecount v)))
    (is (numerical? v))
    (is (== 1 (dimensionality v)))
    (is (== 0 (dimension-count v 0)))))