(ns clojure.core.matrix.test-blas
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.blas :as blas]
            [clojure.core.matrix :as m])
  (:require 
       #?@(:clj [[clojure.core.matrix.macros :refer [error doseq-indexed]]
             [clojure.core.matrix.macros-clj :refer [error?]]
             [clojure.test :refer [deftest is testing run-tests]]]
         :cljs [[cljs.test :refer-macros [deftest is testing run-tests]]
             [clojure.core.matrix :refer-macros [with-implementation]]
             [clojure.core.matrix.macros :refer-macros [error]]
             [clojure.core.matrix.macros-cljs :refer-macros [error?]]]))
  )

(deftest test-gemm!
  (let [a (m/array [[2 3] [5 7]])
        b (m/array [[11 13] [17 19]])
        c (m/array [[1 2] [3 4]])]
    (let [c (m/mutable c)]
      (is (m/equals [[73 83] [174 198]] (blas/gemm! 1.0 a b c))))
    (let [c (m/mutable c)]
      (is (m/equals [[75 87] [180 206]] (blas/gemm! 1.0 a b 2.0 c))))))

(deftest test-gemv!
  (let [a (m/array [[2 3] [5 7]])
        b (m/array [11 17])
        c (m/array [1 2])]
    (let [c (m/mutable c)]
      (is (m/equals [73 174] (blas/gemv! 1.0 a b c))))
    (let [c (m/mutable c)]
      (is (m/equals [75 178] (blas/gemv! 1.0 a b 2.0 c))))))

(deftest test-gemm
  (let [a (m/array [[2 3] [5 7]])
        b (m/array [[11 13] [17 19]])
        c (m/array [[1 2] [3 4]])]
    (is (m/equals [[73 83] [174 198]] (blas/gemm 1.0 a b c)))
    (is (m/equals [[75 87] [180 206]] (blas/gemm 1.0 a b 2.0 c)))
    (is (m/equals [[1 2] [3 4]] c))))

(deftest test-gemv
  (let [a (m/array [[2 3] [5 7]])
        b (m/array [11 17])
        c (m/array [1 2])]
    (is (m/equals [73 174] (blas/gemv 1.0 a b c)))
    (is (m/equals [75 178] (blas/gemv 1.0 a b 2.0 c)))
    (is (m/equals [1 2] c))))

