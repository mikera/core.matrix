(ns clojure.core.matrix.test-sequence
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]))

;; Tests for core.matrix implementation for arbitrary sequences (ISeq)

(deftest regressions
  (is (== 3 (ereduce (fn [acc _] (inc acc)) 0 '(nil nil nil))))
  (is (e== [-1 -2] (negate '(1 2))))
  (is (= '() (mget '()))))

(deftest test-columns
  (let [m '(["A" 2] ["B" 3])]
    (is (e= (columns m) (transpose m)))
    (with-implementation :vectorz
      (is (e= (columns m) (transpose m))))))

(deftest test-sequence-shape
  (is (= [2] (shape '(1 2))))
  (is (= [2 2] (shape '((1 2) (3 4))))))

(deftest sequence-ops
  (is (equals (mul (range 10) (range 10)) '(0 1 4 9 16 25 36 49 64 81))))

;; sequences should get converted to a better implementation
(deftest test-to-vectors
  (is (vector? (emap inc '(1 2 3)))))

(deftest test-emap
  (is (= [2 3 4] (emap inc '(1 2 3))))
  (is (= [5 7 9] (emap + '(1 2 3) '(4 5 6))))
  ;(is (= [7 9 11] (emap + '(1 2 3) '(4 5 6) '(2 2 2)))) ; FIXME throws exception
  )

(deftest test-emap-indexed
  (is (= [2 4 6] (emap-indexed #(+ (reduce + %1) (inc %2)) '(1 2 3))))
  (is (= [5 8 11] (emap-indexed #(apply + (reduce + %1) %&) '(1 2 3) '(4 5 6))))
  ;(is (= [7 10 13] (emap-indexed #(apply + (reduce + %1) %&) '(1 2 3) '(4 5 6) '(2 2 2)))) ; FIXME throws exception
  )

(deftest test-empty-sequence
  (let [v '()]
    (is (== 0 (ecount v)))
    (is (numerical? v))
    (is (== 1 (dimensionality v)))
    (is (== 0 (dimension-count v 0)))))


(deftest compliance-test
  (compliance/compliance-test '(1))
  (compliance/instance-test '())
  (compliance/instance-test '((1 2) (3 4))))
