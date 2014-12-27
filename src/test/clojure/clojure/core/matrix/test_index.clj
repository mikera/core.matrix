(ns clojure.core.matrix.test-index
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :refer :all]))

(deftest test-int-index
  (let [xs (int-array [1 2 3])]
    (testing "Index identity"
	    (is (identical? xs (index xs xs)))
	    (is (index? xs)))))

(deftest test-long-index
  (let [xs (long-array [1 2 3])]
    (testing "Index identity"
	    (is (identical? xs (index xs xs)))
	    (is (index? xs)))))

(deftest test-vector-index
  (let [xs [1 2 3]]
    (testing "Index identity"
	    ;; (is (identical? xs (index xs xs)))
	    (is (index? xs)))))

(deftest test-index-coercions
  (is (index? (index '(1 2 3)))))

