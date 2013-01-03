(ns clatrix.test-api
  (:use clojure.test)
  (:use clatrix.api))

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (get-1d [1 2 3] 0)))
    (is (== 1 (get-2d [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (get-nd [[[1 2] [3 4]] [[5 6] [7 8]]] [1 1 1])))))