(ns clojure.core.matrix.test-select
  "Namespace for testing the clojure.core.matrix.select API"
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [error? broadcast-shape]]
            [clojure.core.matrix.select :refer :all]
            [clojure.test :refer :all]))


(deftest test-set-sel
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12]]]
    (testing "set-sel"
      (is (= [[2 2 3 4] [5 6 7 8] [9 10 11 12]] (set-sel a 0 0 2)))
      (is (= [[3 2 3 3] [5 6 7 8] [3 10 11 3]] (set-sel a [0 2] [0 3] 3))))))

(deftest test-set-sel!
  (let [a (matrix :ndarray [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
    (testing "set-sel!"
      (set-sel! a 0 0 2)
      (is (= [[2 2 3 4] [5 6 7 8] [9 10 11 12]] a))
      (set-sel! a :all 0 0)
      (is (= [[0 2 3 4] [0 6 7 8] [0 10 11 12]] a)))))

(deftest test-selector-functions
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]]
    (is (equals a (sel a (irange) (irange))))
    (is (equals [[5 6 7 8] [9 10 11 12]] (sel a (irange 1 2) :all)))
    (is (equals [2 3 4] (sel a (exclude [1 2 3]) (exclude 0))))
    (is (equals [[1 3] [9 11]] (sel a even even)))
    (is (equals [[6 8] [14 16]] (sel a odd odd)))))



