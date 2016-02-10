(ns clojure.core.matrix.test-selection
  "Namespace for testing the clojure.core.matrix.select API"
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.utils :refer [broadcast-shape]]
   #?(:clj  [clojure.core.matrix.macros-clj :refer [error?]]
      :cljs [clojure.core.matrix.macros-cljs :refer-macros [error?]])
            [clojure.core.matrix.selection :refer [sel where exclude irange set-sel set-sel! odd even end]]
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])))

(deftest test-sel
  (let [a [[1 2] [3 4]]]
    (testing "higher level indexing"
      (is (mat/equals 1 (sel a 0 0)))
      (is (mat/equals [[1] [3]] (sel a [0 1] [0])))
      (is (mat/equals [1 3] (sel a [0 1] 0)))
      (is (mat/equals a (sel a :all :all)))
      (is (mat/equals 4 (sel a end end)))
      (is (mat/equals 2 (sel a (exclude 1) (exclude 0))))
      (is (mat/equals [[1 2]] (sel [[-1 0] [1 2]] (where pos?) :all)))
      (is (mat/equals [0 1 2 3 4] (sel (range 10) (where (partial > 5))))))))

(deftest test-set-sel
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12]]]
    (testing "set-sel"
      (is (mat/equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] (set-sel a 0 0 2)))
      (is (mat/equals [[3 2 3 3] [5 6 7 8] [3 10 11 3]] (set-sel a [0 2] [0 3] 3))))))

(deftest test-set-sel!
  (let [a (mat/matrix :ndarray [[1 2 3 4] [5 6 7 8] [9 10 11 12]])]
    (testing "set-sel!"
      (set-sel! a 0 0 2)
      (is (mat/equals [[2 2 3 4] [5 6 7 8] [9 10 11 12]] a))
      (set-sel! a :all 0 0)
      (is (mat/equals [[0 2 3 4] [0 6 7 8] [0 10 11 12]] a)))))

(deftest test-selector-functions
  (let [a [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]]]
    (is (mat/equals a (sel a (irange) (irange))))
    (is (mat/equals [[5 6 7 8] [9 10 11 12]] (sel a (irange 1 2) :all)))
    (is (mat/equals [2 3 4] (sel a (exclude [1 2 3]) (exclude 0))))
    (is (mat/equals [[1 3] [9 11]] (sel a even even)))
    (is (mat/equals [[6 8] [14 16]] (sel a odd odd)))))

(deftest test-vector-selects
  (let [a [1 2 3 4 5]]
    (is (mat/equals a (sel a :all)))
    (is (mat/equals [1 2 3 4] (sel a :butlast)))
    (is (mat/equals [2 3 4 5] (sel a :rest)))
    (is (error? (sel a -1)))
    (is (mat/equals 1 (sel a 0)))
    (is (mat/equals 5 (sel a 4)))
    (is (error? (sel a 5))))
  (let [a [1]]
    (is (mat/equals a (sel a :all)))
    (is (mat/equals [] (sel a :butlast)))
    (is (mat/equals [] (sel a :rest)))
    (is (error? (sel a -1)))
    (is (mat/equals 1 (sel a 0)))
    (is (error? (sel a 1)))))

