(ns clojure.core.matrix.test-comparisons
  (:refer-clojure :exclude [==])
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer [==]]
            [clojure.core.matrix.utils :refer [error? broadcast-shape]]
            [clojure.test :refer :all])
  (:import [java.io StringWriter]))


(deftest test-comparison
  (testing "element-wise if"
    (let [A [[1 5] [2 3]]
        B [[1 3] [6 3]]
        C [[2 5] [8 2]]
        D [[3 1] [9 4]]]
      (is (== 3 (eif (lt 1 3) 3 6)))
      (is (== 6 (eif (lt 5 3) 3 6)))
      (is (== [[1 2] [2 1]] (eif (eq A B) 1 2)))
      (is (== [[1 1] [9 1]] (eif (eq A B) 1 D)))
      (is (== [[2 2] [2 2]] (eif (eq A B) C 2)))
      (is (== [[2 1] [9 2]] (eif (eq A B) C D)))))
  (testing "element-wise cmp"
    (is (== -1 (cmp 1 3)))
    (is (== 0 (cmp 0 0)))
    (is (== 1 (cmp 1 -1)))
    (is (== [[-1 0] [0 1]] (cmp [[1 3] [5 5]] [[3 3] [5 3]]))))
  (testing "element-wise lt"
    (is (== 1 (lt 1 4)))
    (is (== 0 (lt 3 3)))
    (is (== [[1 0] [0 0]] (lt [[1 5] [3 6]] 3)))
    (is (== [[1 0] [1 0]] (lt [[1 5] [4 6]] [[2 3] [5 6]]))))
  (testing "element-wise le"
    (is (== 1 (le 3 3)))
    (is (== 0 (le 4 3)))
    (is (== [[1 0] [1 0]] (le [[1 5] [3 6]] 3)))
    (is (== [[1 0] [1 1]] (le [[1 5] [4 6]] [[2 3] [5 6]]))))
  (testing "element-wise gt"
    (is (== 1 (gt 4 3)))
    (is (== 0 (gt 3 3)))
    (is (== [[0 1] [0 1]] (gt [[1 5] [3 6]] 3)))
    (is (== [[0 1] [0 0]] (gt [[1 5] [4 6]] [[2 3] [5 6]]))))
  (testing "element-wise ge"
    (is (== 0 (ge 2 3)))
    (is (== 1 (ge 3 3)))
    (is (== [[0 1] [1 1]] (ge [[1 5] [3 6]] 3)))
    (is (== [[0 1] [0 1]] (ge [[1 5] [4 6]] [[2 3] [5 6]]))))
  (testing "element-wise ne"
    (is (== 0 (ne 1 1)))
    (is (== 1 (ne 5 1)))
    (is (== [[1 1] [0 1]] (ne [[1 5] [3 6]] 3)))
    (is (== [[1 1] [1 0]] (ne [[1 5] [4 6]] [[2 3] [5 6]]))))
  (testing "element-wise eq"
    (is (== 1 (eq 1 1)))
    (is (== 0 (eq 5 1)))
    (is (== [[0 0] [1 0]] (eq [[1 5] [3 6]] 3)))
    (is (== [[0 0] [0 1]] (eq [[1 5] [4 6]] [[2 3] [5 6]])))))

(deftest test-broadcast-comparisons
  (testing "element-wise if"
    (is (= [false false true] (eif [-1 0 1] true false))))
  (testing "vector broadcasting to matrix"
    (is (== [[1 0] [0 0]] (lt [[1 5] [3 6]] [3 3])))
    (is (== [[1 0] [1 0]] (le [[1 5] [3 6]] [3 3])))
    (is (== [[0 1] [0 1]] (gt [[1 5] [3 6]] [3 3])))
    (is (== [[0 1] [1 1]] (ge [[1 5] [3 6]] [3 3])))
    (is (== [[1 1] [0 1]] (ne [[1 5] [3 6]] [3 3])))
    (is (== [[0 0] [1 0]] (eq [[1 5] [3 6]] [3 3]))))
  (testing "number as second argument"
    (is (== [-1 0 1] (cmp [0 1 2] 1)))
    (is (== [1 0 0] (lt [1 2 3] 2)))
    (is (== [1 1 0] (le [1 2 3] 2)))
    (is (== [0 0 1] (gt [1 2 3] 2)))
    (is (== [0 1 1] (ge [1 2 3] 2)))
    (is (== [1 0 1] (ne [1 2 3] 2)))
    (is (== [0 1 0] (eq [1 2 3] 2))))
  (testing "number as first argument"
    (is (== [-1 0 1] (cmp 1 [2 1 0])))
    (is (== [0 0 1] (lt 2 [1 2 3])))
    (is (== [0 1 1] (le 2 [1 2 3])))
    (is (== [1 0 0] (gt 2 [1 2 3])))
    (is (== [1 1 0] (ge 2 [1 2 3])))
    (is (== [1 0 1] (ne 2 [1 2 3])))
    (is (== [0 1 0] (eq 2 [1 2 3])))))