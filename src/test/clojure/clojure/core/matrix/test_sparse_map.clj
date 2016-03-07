(ns clojure.core.matrix.test-sparse-map
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :as mat]
            [clojure.test :refer [deftest testing is run-tests]]))

(defn sm
  [data]
  (mat/matrix :persistent-map data))

(deftest test-new
  (is (mat/equals [0] (mat/new-vector :persistent-map 1))))

(deftest test-sparse-map-construct
  (let [m (sm [[1 2] [3 4]])]
    (is (== 2 (mat/dimensionality m)))
    (is (mat/e= [[1 nil] [nil 1]] ^{:shape [2 2]} {[0 0] 1 [1 1] 1}))
    (is (mat/equals [[1 0] [0 1]] ^{:shape [2 2] :default-value 0} {[0 0] 1 [1 1] 1}))))

(deftest instance-tests
  (testing "matrices of symbols are supported"
    (compliance/instance-test (sm ['a 'b])))
  (testing "matrices of heterogeneous submatrices"
    (compliance/instance-test (sm [[1 2.0] (double-array [3 4])]))))

(deftest compliance-test
  (compliance/compliance-test (sm [[1]])))

(defn test-vector-products []
  (let [m ^{:shape [2 2]} {[0 0] 1 [1 1] 1}
        a (mat/matrix m [1 2 3])
        b (mat/matrix m [4 5 6])]
    (is (mat/equals [[4 5 6] [8 10 12] [12 15 18]] (mat/outer-product a b)))
    (is (mat/equals 32 (mat/inner-product a b)))))

