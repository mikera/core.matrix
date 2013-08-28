(ns clojure.core.matrix.test-sparse-map
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [clojure.core.matrix.impl.sparse-map :as sm]))

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
    (clojure.core.matrix.compliance-tester/instance-test (sm ['a 'b])))
  (testing "matrices of heterogeneous submatrices"
    (clojure.core.matrix.compliance-tester/instance-test (sm [[1 2.0] (double-array [3 4])]))))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (sm [[1]])))