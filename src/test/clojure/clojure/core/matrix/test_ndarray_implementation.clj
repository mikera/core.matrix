(ns clojure.core.matrix.test-ndarray-implementation
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.compliance-tester :as ct])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.generic :as gen])
  (:require clojure.core.matrix.impl.persistent-vector)
  (:use clojure.core.matrix.impl.ndarray))

(deftest c-strides-test
  (are [strides shape] (= strides (c-strides shape))
       [1]        [3]
       [2 1]      [3 2]
       [6 2 1]    [4 3 2]
       [24 6 2 1] [5 4 3 2]))

(deftest f-strides-test
  (are [strides shape] (= strides (f-strides shape))
       [1]         [3]
       [1 3]       [3 2]
       [1 4 12]    [4 3 2]
       [1 5 20 60] [5 4 3 2]))

(deftest empty-ndarray-test
  (let [a (empty-ndarray [3 2])]
    (is (= [nil nil nil nil nil nil] (vec (.data a))))
    (is (= 2 (.ndims a)))
    (is (= [3 2] (vec (.shape a))))
    (is (= [2 1] (vec (.strides a))))))

(deftest regressions
  (let [m (empty-ndarray [2 2])
        vm [[1 2] [3 4]]]
    (is (thrown? Throwable (esum m)))
    (assign! m vm)
    (is (= vm (coerce [] m)))
    (is (== 10 (esum m))))
  (testing "esq and ereduce"
    (let [m (matrix :ndarray [3])
          es (eseq m)]
      (is (== 1 (count es)))
      (is (== 3 (mget m 0)))
      (is (== 3 (ereduce + 0 m)))
      (is (== 3 (reduce + 0 es))))
    (let [m (matrix :ndarray [[1 2] [3 4]])
          es (eseq m)]
      (is (== 10 (ereduce + m)))
      (is (== 4 (ereduce (fn [acc _] (inc acc)) 0 m)))
      (is (== 4 (ereduce (fn [acc _] (inc acc)) 0 (eseq m))))))

  (deftest test-ndarray-base
    (testing "construction"
      (is (= [3 3] (seq (shape (empty-ndarray [3 3]))))))
    (testing "getters"
      (is (= nil (mget (empty-ndarray [3 3]) 2 2)))
      (is (= nil (mget (empty-ndarray [3 3 3]) 1 1 1))))
    (testing "setters"
      (let [m (empty-ndarray [2 2])]
        (mset! m 0 0 1)
        (is (== 1.0 (mget m 0 0)))))
    (testing "slices"
      (is (= [[nil nil] [nil nil]]
             (map #(coerce [] %)
                  (slices (empty-ndarray [2 2]))))))))

(deftest test-contained-vectors
  (let [a (array :ndarray :foo)]
    (mset! a [1 2 3])
    (is (== 1 (ecount a)))
    (is (= [1 2 3] (mget a)))
    (is (= [] (shape a)))
    (is (equals 3 (emap count a)))))

(deftest test-seq
  (is (= (-> (array :ndarray [[1 2] [3 4]])
             seq
             second
             mp/persistent-vector-coerce)
         [3 4])))

(deftest test-assign
  (let [m (empty-ndarray [2 2 2])
        vm [[[0 1] [2 3]] [[4 5] [6 7]]]]
    (assign! m vm)
    (is (= (eseq m) (range 8)))))

(deftest test-object-emap
  (let [m (new-array :ndarray [2 2])
        vecs (for [i (range 4)] [i (inc i)])]
    (assign-array! m (object-array vecs))
    (is (equals [[2 2] [2 2]] (emap count m)))))

#_(deftest test-helper-functions
  (is (== 35 (calc-index [1 5] (long-array [100 30]))))
  (is (== 10101 (calc-index [1 1 1] (long-array [200 100 100])))))

(deftest test-transpose
  (let [m (new-array :ndarray [5 6 7])]
    (is (= [5 6 7] (seq (shape m))))
    (is (= [7 6 5] (seq (shape (transpose m)))))))

(defn get-primitive-ndarrays []
  [(empty-ndarray-double [3 3])
   (empty-ndarray-long [3 3])
   (empty-ndarray-float [3 3])])

(deftest default-values
  (is (nil? (gen/default-value :ndarray)))
  (is (= 0.0 (gen/default-value :ndarray-double)))
  (is (= 0 (gen/default-value :ndarray-long))))

(deftest ndarray-test
  (ct/test-ndarray-implementation (empty-ndarray [3 3])))

;; run complicance tests
(deftest compliance-test
  (ct/compliance-test (empty-ndarray [3 3])))

(deftest compliance-test-primitives
  (doseq [m (get-primitive-ndarrays)]
    (ct/compliance-test m)))
