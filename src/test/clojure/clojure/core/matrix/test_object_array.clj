(ns clojure.core.matrix.test-object-array
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]))

;; Tests for core.matrix functions on Java Object [] arrays
;;
;; This is an important implementation because it provides efficient support
;; for mutable vectors of arbitrary objects on the JVM

(deftest regressions
  (is (= [2] (seq (emap inc (object-array [1]))))))

(deftest test-functional-ops
  (testing "map"
    (let [oa  (object-array [1 2])
          oa2 (object-array [3 4])
          oa3 (object-array [5 6])]
      (is (= [2 3] (seq (emap inc oa))))
      (is (= [4 6] (seq (emap + oa oa2))))
      (is (= [9 12] (seq (emap + oa oa2 oa3)))))))

(deftest to-objects
  (is (equals [0 1 2] (to-object-array (range 3))))
  (is (e= [1 2 3 :foo] (to-object-array [[1 2] [3 :foo]]))))

(deftest instance-tests
  ;(clojure.core.matrix.compliance-tester/instance-test (object-array []))
  (compliance/instance-test (object-array [1]))
  (compliance/instance-test (object-array [1 :foo]))
  (compliance/instance-test (object-array [-1 4 2 7 -3])))

(deftest compliance-tests
  (compliance/compliance-test (object-array [0.23])))
