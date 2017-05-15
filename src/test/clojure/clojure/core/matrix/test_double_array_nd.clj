(ns clojure.core.matrix.test-double-array-nd
  (:refer-clojure :exclude [==])
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.compliance-tester]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.operators :refer [==]]
            [clojure.core.matrix.utils :refer :all]
            [clojure.core.matrix.impl.double-array :refer :all]))

(def golden-ratio 1.61803398875)
(def default-value [[1.0 2.0 3.0]
                    [4.0 5.0 6.0]])

(defmacro wrap-test
  ([clause m & body]
  `(testing ~clause
    (let [~m (matrix :double-array default-value)]
      ~@body))))

; utilities test
(deftest test-copy-2d-double-array
  (let [m (matrix :double-array [[1.0 2.0 3.0] [4.0 5.0 6.0]])
        m2 (copy-2d-double-array m)]
    (is (== m m2))
    (aset-double m2 0 0 23.0)
    (is (not (== m m2)))))

; == Specification tests (dimensionality and types)
(deftest test-element-type
  (= Double/TYPE (element-type (matrix :double-array [[1]]))))

(deftest test-dimensionality
  (== 2 (dimensionality (matrix :double-array [[1]]))))

(deftest test-is-mutable-matrix
  (is (true? (mutable? (matrix :double-array [[1]])))))

; == core API tests
(deftest test-create
  (testing "Making a 2D double array"
    (let [da (matrix :double-array [[1 2]])]
      (is (= (mapv seq da) [[1.0 2.0]]))
      (is (= (eseq da) [1.0 2.0]))
      (is (= (Class/forName "[[D") (class da)))))
  (testing "coerce from a perssistent vector"
    (let [da (matrix :double-array [[1 2]])]
      (is (= [[2.0 4.0]] (mapv seq (coerce da [[2 4]]))))
      (is (= (class da) (class (coerce da [[2 4]])))))))

(deftest test-setting
  (testing "row setting"
    (let [da (matrix :double-array [[1 2 3] [4 5 6]])]
      (set-row! da 0 [7.0 8.0 9.0])
      (is (== da [[7.0 8.0 9.0] [4.0 5.0 6.0]]))))
  (testing "column setting"
    (let [da (matrix :double-array [[1 2 3] [4 5 6]])]
      (set-column! da 1 [10 11])
      (is (== da [[1.0 10.0 3.0] [4.0 11.0 6.0]])))))

(comment
  (deftest test-array-outputs
    (wrap-test "test object array outputs" m
               (let [m2 (mp/to-object-array m)]
                 (is (not= m m2))
                 (is (== m m2))
                 (is (= (class m2) (class (make-array Object 1 1))))))
    (wrap-test "test double array outputs" m
               (let [m2 (mp/to-double-array m)]
                 (is (not= m m2))
                 (is (== m m2)))
               (let [m2 (mp/as-double-array m)]
                 (is (= m m2))))))

(deftest test-summable
  (testing "element sum"
    (let [da (matrix :double-array [[1 2] [3 4]])]
      (is (== (esum da) 10.0)))))

(deftest indexed-access
  (wrap-test "Test indexed access" m
    (is (== 6.0 (mget m 1 2)))))

(deftest indexed-setting
  (wrap-test "Test mutable indexed setting" m
    (mset! m 0 1 25.0)
    (is (== 25.0 (mget m 0 1))))
  (wrap-test "Test imutable indexed setting" m
    (let [m2 (mset m 0 1 25.0)]
      ; verify that it returns a matrix, which is not equal to the old one
      (is (= (class m2) (Class/forName "[[D")))
      (is (not= m m2))
      (is (== m2 [[1.0 25.0 3.0]
                  [4.0 5.0 6.0]]))
      (is (not (== 25.0 (mget m 0 1)))))))
      ; make sure the original matrix was not mutated

(deftest test-matrix-scaling
  (wrap-test "Test scaling" m
    (let [m2 (scale m 2)]
      (is (== m2 (emap #(* 2 %) default-value)))
      (is (not= m2 (emap #(* 2 %) default-value)))))
  (wrap-test "Test mutable scaling" m
    (scale! m 2)
    (is (== m (emap #(* 2 %) default-value)))))

(deftest test-conversion
  (wrap-test "Test convert to-nested-vectors" m
    (is (= (emap double default-value) (to-nested-vectors m))))
  (wrap-test "Test convert coerce-param" m
    (let [m2 (coerce m [[1 2 3] [4 5 6]])]
      ; m and m2 should reference to differnt objects
      (is (not= m m2))
      (is (= (class m) (class m2)))
      (is (== m m2)))))

(deftest test-vector-ops
  (wrap-test "Test dot product with 1D vector" m
    (is (== (dot m [1 2 3])
            (mapv #(apply + (mapv * % [1 2 3]))
                  m))))
  (wrap-test "Test dot product with 2D array" m
    (is (== (dot m m)
            (mapv #(apply + (mapv * %1 %2))
                  m m)))))

(deftest test-matrix-cloning
  (wrap-test "Test cloning" m
             (let [m2 (clone m)]
               (is (= (class m) (class m2)))
               (is (not= m m2))
               (is (== m m2)))))

(deftest test-functional-ops
  (wrap-test "test element-seq" m
             (is (== (eseq m) [1 2 3 4 5 6])))
  (wrap-test "test element-map" m
             (is (== (emap #(* 2 %) m)
                     (emap #(* 2 %) default-value)))
             (is (== (emap * m m)
                     (emap * default-value default-value)))
             (is (== (emap * m m m m)
                     (emap #(* % % % %) default-value))))
  (wrap-test "test element-map!" m
             (emap! #(* 2 %) m)
             (is (== m (emap #(* % 2) default-value))))
  (wrap-test "test element-reduce" m
             (is (== (ereduce + 0 m) (apply + (mapv #(apply + %)
                                              default-value))))))

(defn indices-matrix
  "Returns a matrix where the value of every position
  corresponds to (+ i j), where i is the first dimension's
  index and j is the second dimension's index"
  ([[x y]]
   (indices-matrix x y))
  ([x y]
  (mapv (fn [i]
          (mapv (fn [j] (+ i j)) (range y)))
        (range x))))

(deftest test-map-indexed
  (wrap-test "test emap-indexed" m
             (dotimes [n 5]
               (when (>= n 1)
                 (is (== (apply emap-indexed (fn [[i j] & v] (+ i j)) (repeat n m))
                         (indices-matrix (mp/get-shape default-value)))))))
  (wrap-test "test emap-indexed!" m
             (emap-indexed! (fn [[i j] _] (+ i j)) m)
             (is (== m
                     (indices-matrix (mp/get-shape default-value))))))

(deftest test-mmul 
  (is (= 14.0 (mmul (double-array [1 2 3]) (double-array [1 2 3]))))
  (is (error? (mmul (double-array [1 2 3]) (double-array [1 2 3 4]))))
  (is (equals [14 20] (mmul (matrix :double-array [[1 2 3] [2 3 4]]) (double-array [1 2 3])))))

(deftest test-matrix-divide
  (wrap-test "test div" m
             (is (== (div m golden-ratio)
                     (emap #(/ % golden-ratio) default-value))
                 (== (div m)
                     (emap / default-value))))
  (wrap-test "test div!" m
             (div! m golden-ratio)
             (is (== m (emap #(/ % golden-ratio)
                             default-value)))))

(deftest test-select
  (wrap-test "test select" m
    (let [selected (select m 0 0)]
      (is (scalar? selected))
      (is (or (instance? Double/TYPE selected)
              (instance? java.lang.Double selected))))
    (let [selected (select m [0] [0])]
      (is (instance? (Class/forName "[[D") selected)))
    (let [selected (select m 0 :all)]
      (is (= 1 (mp/dimensionality selected)))
      (is (== selected [1.0 2.0 3.0]))
      (is (instance? (Class/forName "[D") selected)))))

(deftest instance-test
  (clojure.core.matrix.compliance-tester/instance-test (construct-double-array [[1 1 2]
                                                                                [-13 -8 -5]]))
  (clojure.core.matrix.compliance-tester/instance-test (construct-double-array [[[1 1 2]
                                                                                 [-13 -8 -5]]])))

; TODO: complete 2d double array impl.  element-map doesn't work as is when
; doing an outer-product because it's trying to cast an array as a double...
(comment deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test (construct-double-array [[0.23]])))
