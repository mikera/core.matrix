(ns clojure.core.matrix.test-row-operators
  (:use clojure.test)
  (:use clojure.core.matrix.row-operators))


(deftest test-swap
  (testing "vector row swap"
    (is (= [0 2] (swap [2 0] 0 1)))
    (is (= [0 2] (swap [2 0] 1 0))))
  (testing "matrix row swap"
    (is (= [[0 2] [2 0]] (swap [[2 0] [0 2]] 0 1)))
    (is (= [[0 2] [2 0] [1 1]] (swap [[1 1] [2 0] [0 2]] 0 2)))))

(deftest test-multiply
  (testing "multiply row i by constant k"
    (is (= [[0 2 4]] (multiply [[0 1 2]] 0 2)))))

(deftest test-add
  (testing "add row j to i and replace i with the result"
    (is (= [[3 3] [1 1]] (add [[1 1] [1 1]] 0 1 2 )))))
