(ns clojure.core.matrix.impl.test-wrappers
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.impl.wrappers)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester]))

(deftest regressions
  (is (str (wrap-slice [[1 2] [3 4]] 1))))

(deftest assumptions
  (is (== 0 (dimensionality (wrap-nd 7))))
  (is (== 1 (dimensionality (wrap-nd [7]))))
  (is (not (scalar? (wrap-nd 7))))
  (is (not (vec? (wrap-nd 7))))
  (is (vec? (wrap-nd [7])))
  (is (equals 7 (wrap-nd 7)))
  (is (nil? (seq (shape (wrap-nd 7)))))
  (is (= [2] (eseq (wrap-scalar 2))))
  (is (= [2] (eseq (emap inc (wrap-scalar 1))))))

(deftest test-mutability
  (let [s (wrap-scalar 7)]
    (is (== 10 (mget (mset! s 10))))
    (is (== 10 (mget s))))
  (let [s (wrap-scalar 7)]
    (is (== 8 (mget (emap! inc s))))
    (is (== 8 (mget s))))
  (let [s (wrap-scalar 7)]
    (scale! s 2)
    (is (== 14 (mget s))))
  (let [v (wrap-nd (double-array [1 2]))]
    (is (equals [3 12] (add v [2 10])))
    ;(add! v [2 10])  TODO: fix mutation of NDWRapper
    ;(is (equals [3 12] v))
  ))

(deftest scalar-assumptions
  (is (== 0 (dimensionality (wrap-scalar 7))))
  (is (not (scalar? (wrap-scalar 7))))
  (is (not (vec? (wrap-scalar 7))))
  (is (mutable? (wrap-scalar 7)))
  (is (mutable? (wrap-nd (wrap-scalar 7))))
  (is (zero-dimensional? (wrap-scalar 7))))

(deftest test-wrapped-slice
  (let [m (wrap-slice [[1 2] [3 4]] 1)]
    (equals [4 5] (emap inc m))))

(deftest test-wrapped-subvector
  (let [m [1 2 3 4]]
    (equals [2 3] (subvector m 1 2))))

(deftest test-wrapped-nd
  (let [m (wrap-nd [3 4])]
    (equals [4 5] (emap inc m))))

(deftest test-slice-wrap
  (is (equals [3 4] (wrap-slice [[1 2] [3 4]] 1)))
  (is (equals 3 (wrap-slice [3 4] 0))))

(deftest test-nd-wrap
  (is (equals 1 (wrap-nd 1)))
  (is (equals [3 4] (wrap-nd [3 4])))
  (is (equals [3 4] (seq (wrap-nd [3 4])))))

(deftest test-as-vector
  (is (e== [1] (as-vector (wrap-scalar 1)))))

(deftest test-scalar-construct-vector
  (is (e== [1] (coerce (wrap-scalar 2) [1]))))

(deftest test-nd-slice
  (let [ss (second (slices (wrap-nd [[3 4] [5 6]])))]
    (is (equals [5 6] ss))
    (is (== 1 (dimensionality ss)))
    (is (= [2] (seq (shape ss)))))
  (let [ss (mp/get-major-slice-seq (wrap-nd [[3 4] [5 6]]))]
    (is (equals [3 4] (first ss)))
    (is (= [3 4] (coerce [] (first ss))))))

(deftest test-nd-transpose
  (is (equals 3 (transpose (wrap-nd 3))))
  (is (equals [3 4] (transpose (wrap-nd [3 4]))))
  (let [m (wrap-nd [[1 2] [3 4]])]
    (is (equals [[1 2] [3 4]] (mp/get-major-slice-seq m)))
    (is (= [[1 2] [3 4]] (map #(mp/coerce-param [] %) (mp/get-major-slice-seq m)))))
  (is (equals [[1 3] [2 4]] (transpose (wrap-nd [[1 2] [3 4]])))))

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test (wrap-scalar 1))
  (clojure.core.matrix.compliance-tester/instance-test (wrap-slice [[1 2] [3 4]] 1))
  (clojure.core.matrix.compliance-tester/instance-test (wrap-submatrix [[1 2] [3 4]] [[1 1] [0 1]]))
  (clojure.core.matrix.compliance-tester/instance-test (wrap-nd [[1 2] [3 4]])))

