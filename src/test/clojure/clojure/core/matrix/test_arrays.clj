(ns clojure.core.matrix.test-arrays
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :as m]
            ;[clojure.core.matrix.utils :refer :all]
            [clojure.test :refer [deftest testing is]]
            [clojure.core.matrix.macros :refer [array?]]))

;; This namespace is intended for tests of core.matrix functions on arbitrary Java arrays
;;
;; should use ints, floats etc. where possible (since doubles and Objects are tested elsewhere)

(deftest int-array-test
  (let [a (int-array [1 2 3])]
    (is (== 1 (m/dimensionality a)))
    (is (m/vec? a))
    (is (== 6 (m/esum a)))
    (is (m/equals (m/pow a a) [1 4 27])))
  (is (m/orthogonal? [(int-array [0 1]) (int-array [1 0])])))

(deftest array-slicing
  (let [a (long-array [1 2 3])]
    (is (== 1 (first (m/slices a)))))
  (let [a (object-array [1 2 3])]
    (is (array? (first (m/slice-views a))))))

(deftest zero-dimension-access
  (testing "Arrays are assumed to be scalars if accessed with zero dimensions"
    (is (m/equals [1] (m/mget (byte-array [1]))))
    (is (m/equals [1] (m/mget (double-array [1]))))))

(deftest compliance-tests
  (compliance/instance-test (int-array [1 2 3]))
  (compliance/instance-test (float-array [1 2 3]))
  (compliance/instance-test (long-array []))
  (compliance/instance-test (char-array [\a \b \c]))
  (compliance/instance-test (object-array [(double-array [1 2 3])]))
  (compliance/instance-test (object-array [(short-array (map short [1 2 3]))]))
  (compliance/instance-test (into-array (Class/forName "[D")
                                        [(double-array [1 2])
                                         (double-array [3 4])])))
