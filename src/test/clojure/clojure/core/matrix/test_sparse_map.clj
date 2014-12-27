(ns clojure.core.matrix.test-sparse-map
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]))

(defn sm
  [data]
  (matrix :persistent-map data))

(deftest test-new
  (is (equals [0] (new-vector :persistent-map 1))))

(deftest test-sparse-map-construct
  (let [m (sm [[1 2] [3 4]])]
    (is (== 2 (dimensionality m)))
    (is (e= [[1 nil] [nil 1]] ^{:shape [2 2]} {[0 0] 1 [1 1] 1}))
    (is (equals [[1 0] [0 1]] ^{:shape [2 2] :default-value 0} {[0 0] 1 [1 1] 1}))))

(deftest instance-tests
  (testing "matrices of symbols are supported"
    (compliance/instance-test (sm ['a 'b])))
  (testing "matrices of heterogeneous submatrices"
    (compliance/instance-test (sm [[1 2.0] (double-array [3 4])]))))

(deftest compliance-test
  (compliance/compliance-test (sm [[1]])))
