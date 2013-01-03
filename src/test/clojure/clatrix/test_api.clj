(ns clatrix.test-api
  (:use clojure.test)
  (:use clatrix.api))

(deftest test-indexed-access
  (testing "clojure vector indexed access"
     (is (== 1 (mget-1 [1 2 3] 0)))
     (is (== 1 (mget-2 [[1 2 3] [4 5 6]] 0 0)))))