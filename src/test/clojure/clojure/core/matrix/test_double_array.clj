(ns clojure.core.matrix.test-double-array
  (:refer-clojure :exclude [==])
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.impl.pprint :as pprint]
            [clojure.core.matrix.dataset :as ds]
            [clojure.core.matrix.compliance-tester]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer [==]]
            [clojure.core.matrix.macros :refer [error]]
            [clojure.core.matrix.macros-clj :refer [error?]]
            [clojure.test :refer :all]))

;; This namespace contains tests for the Java double[] array implementation
;;
;; This is an important implementation, as it provides maximum efficiency for many
;; numerical vector operations with mutable vectors

(deftest misc-regressions
  (testing "shape sequnces"
    (is (= [0] (shape (double-array []))))
    (is (= [1 0] (shape [(double-array [])])))
    (is (= [0] (shape (transpose (double-array [])))))))

(deftest test-create
  (testing "making a double array"
    (let [da (matrix :double-array [1 2])]
      (is (equals [1.0 2.0] (seq da)))
      (is (e= [1.0 2.0] (eseq da)))
      (is (identical? (class (double-array [1])) (class da)))))
  (testing "coercion from persistent vector"
    (let [da (matrix :double-array [1 2])]
      (is (e= [2.0 4.0] (seq (coerce da [2 4]))))
      (is (identical? (class da) (class (coerce da [2 4])))))))

(deftest test-higher-dimensions
  (let [m [[[1 2] [3 4]]]
        dm (array :double-array m)]
    (is (equals m dm)))
  (is (= 1.0 (array :double-array 1))))

(deftest test-type
  (is (= Double/TYPE (element-type (double-array [1 2])))))

(deftest row-setting
  (let [a (double-array [1 2 3])]
    (is (equals [1 10 3] (set-row a 1 10)))
    (set-row! a 0 7)
    (is (equals [7 2 3] (seq a)))))

(deftest column-setting
  (let [a [(double-array [1 2]) (double-array [3 4])]]
    (is (equals [[1.0 10.0] [3.0 10.0]] (set-column a 1 10)))))

(deftest test-slices
  (testing "slices"
    (let [m [(double-array [1 2]) (double-array [3 4])]]
      (is (equals [1 2] (get-row m 0)))
      (is (equals [2 4] (get-column m 1)))
      (is (e= [1.0 2.0] (slices (get-row m 0))))
      (is (e= [2.0 4.0] (slices (get-column m 1)))))))

(deftest test-scalar-slices
  (testing "slices"
    (let [da (double-array [1 2 3])
          fs (first (slices da))]
      (is (scalar? fs))
      (is (== 0 (dimensionality fs)))
      (is (not (array? fs)))))
  (testing "slice views"
    (let [da (double-array [1 2 3])
          fs (first (slice-views da))]
      (is (not (scalar? fs)))
      (is (== 0 (dimensionality fs)))
      (fill! fs 10)
      (is (equals [10 2 3] da))
      (is (array? fs))))
  (testing "wrong dimension"
    (let [da (double-array [1 2 3])]
      (is (error? (slice da 1 1))))))

(deftest test-functional-ops
  (testing "mapping"
    (let [da (matrix :double-array [1 2])]
      (is (= [2.0 3.0] (seq (emap inc da))))
      (emap! inc da)
      (is (= [2.0 3.0] (vec da)))))
  (testing "nested double arrays"
    (is (= [1.0 2.0 3.0 4.0] (eseq [(double-array [1 2]) (double-array [3 4])]))))
  (testing "mapping indexed"
    (let [da  (matrix :double-array [4 5])
          da2 (matrix :double-array [6 7])
          da3 (matrix :double-array [8 9])]
      (is (= [5.0 7.0]   (seq (emap-indexed #(+ (reduce + %1) (inc %2)) da))))
      (is (= [10.0 13.0] (seq (emap-indexed #(apply + (reduce + %1) %&) da da2))))
      (is (= [18.0 22.0] (seq (emap-indexed #(apply + (reduce + %1) %&) da da2 da3)))))))

(deftest test-assign
  (testing "assign from a persistent vector"
    (let [da (double-array [1 2])]
      (assign! da [2 3])
      (is (= [2.0 3.0] (seq da)))))
  (testing "assign from an array"
    (let [da (double-array [1 2])]
      (assign! da (double-array [2 4]))
      (is (= [2.0 4.0] (seq da)))))
  (testing "assign from a Number array"
    (let [da (double-array [1 2])]
      (mp/assign-array! da (into-array Number [2 5]))
      (is (= [2.0 5.0] (seq da))))))

(deftest test-equals
  (testing "equality with persistent vector"
    (let [da (double-array [1 2])]
      (is (= [1.0 2.0] (to-nested-vectors da)))
      (is (equals [1.0 2.0] da))
      (is (equals [1 2] da))
      (is (equals da [1.0 2.0])))))

(deftest test-add-scaled
  (let [da (double-array [1 2])]
    (is (equals [11 22] (add-scaled da [1 2] 10)))
    (is (equals [101 202] (add-scaled! da [1 2] 100)))
    (is (equals [101 202] da))))

(deftest test-add-scaled-product
  (let [da (double-array [1 2])]
    (is (equals [2 6] (add-scaled-product da [1 2] [1 2] 1)))
    (is (equals [101 202] (add-scaled-product! da [1 2] [10 10] 10)))
    (is (equals [101 202] da))))

(deftest test-vector-scale
  (testing "scale!"
    (let [da (double-array [1.0 2.0])]
      (is (equals [2.0 4.0] (scale! da 2))))))

(deftest test-vector-normalise
  (testing "normalise!"
    (let [da (double-array [4.0])]
      (is (equals [1.0] (normalise! da))))))

(deftest test-mutable-add
  (let [v (double-array [1 2 3])]
    (add! v [10 10 10])
    (sub! v [1 1 2])
    (is (equals v [10 11 11]))))

(deftest test-mutable-map!
  (let [v (double-array [1 2 3])]
    (emap! clojure.core/+ v [10 10 10])
    (is (equals v [11 12 13]))))

(deftest test-div
  (is (equals [1 2] (div (double-array [2 4]) 2))))

(deftest test-div!
  (let [da (double-array [2 4])]
    (div! da 2)
    (is (equals [1 2] da))))

(deftest test-broadcast-coerce
  (is (= [1.0 2.0] (mp/broadcast-coerce [0 0] (double-array [1 2])))))

(deftest test-to-double-arrays
  (let [m [[1 2] [3 4]]
        dm (clojure.core.matrix.impl.double-array/to-double-arrays m)]
    (is (equals m dm)))
  (is (= 1.0 (clojure.core.matrix.impl.double-array/to-double-arrays 1))))

(deftest test-mutable-multiply
  (let [a (double-array [1 2])
        b (double-array [2 3])]
    (is (identical? a (mul! a b)))
    (is (equals [2.0 6.0] (vec a)))
    (is (equals [2.0 6.0] a))))

(deftest test-scale-add
  (let [v (double-array [1 2 3])]
    (scale-add! v 2.0 [10 20 30] 3 100)
    (is (equals [132 164 196] (array [] v)))
    (scale-add! v 0.0 [1 2 3] 2 [-1 -10 10])
    (is (equals [1 -6 16] v))))

(deftest test-element-ops
  (let [a (double-array [5 1 7 8 4])]
    (is (== 1 (emin a)))
    (is (== 8 (emax a)))
    (is (== 25 (esum a)))))

(deftest test-doubles-outer-product
  (let [a (double-array [1 3]) 
        b (double-array [1 2])]
    (is (equals [[1 2] [3 6]] (outer-product a b)))))

(deftest test-maths-ops
  (testing "basic ops"
    (let [da (double-array [1.2 2.3])]
      (is (equals [1.0 2.0] (floor da))))))

(deftest test-add-emap
  (testing "Unary"
    (let [dest (double-array [1 10])
          a (double-array [1.2 2.3])]
      (is (equals [3.4 14.6] (add-emap! dest (partial * 2) a)))))
  (testing "Binary"
    (let [dest (double-array [1 10])
          a (double-array [1.2 2.3])]
      (is (equals [3.4 14.6] (add-emap! dest + a a))))))

(deftest test-set-emap
  (testing "Unary"
    (let [dest (double-array [1 10])
          a (double-array [1.2 2.3])]
      (is (equals [2.4 4.6] (set-emap! dest (partial * 2) a)))))
  (testing "Binary"
    (let [dest (double-array [1 10])
          a (double-array [1.2 2.3])]
      (is (equals [2.4 4.6] (set-emap! dest + a a))))))

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (double-array []))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [1]))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [1 2]))
  (clojure.core.matrix.compliance-tester/instance-test (double-array [-1 4 2 7 -3])))

(deftest test-select
  (testing "select ops"
    (let [da (double-array [1.2 3.4 5.6 7.8 9.1])]
      (let [selected (select da :all)]
        (is (= (class selected) (Class/forName "[D")))
        (is (== selected da))
        (is (not= selected da)))
      (let [selected (select da 0)]
        (is (scalar? selected))
        (is (= 1.2 selected)))
      (let [selected (select da [1 2])]
        (is (= (class selected) (Class/forName "[D")))
        (is (== selected [3.4 5.6]))))))

; TODO: complete 2d double array impl.  element-map doesn't work as is when
; doing an outer-product because it's trying to cast an array as a double...
(comment deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (double-array [0.23])))
