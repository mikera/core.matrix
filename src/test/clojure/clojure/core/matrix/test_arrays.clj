(ns clojure.core.matrix.test-arrays
  (:refer-clojure :exclude [vector?])
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer :all]
            [clojure.test :refer :all]))

;; This namespace is intended for tests of core.matrix functions on arbitrary Java arrays
;;
;; should use ints, floats etc. where possible (since doubles and Objects are tested elsewhere)

(deftest int-array-test
  (let [a (int-array [1 2 3])]
    (is (== 1 (dimensionality a)))
    (is (vec? a))
    (is (== 6 (esum a)))
    (is (equals (pow a a) [1 4 27])))
  (is (orthogonal? [(int-array [0 1]) (int-array [1 0])])))

(deftest array-slicing
  (let [a (long-array [1 2 3])]
    (is (== 1 (first (slices a)))))
  (let [a (object-array [1 2 3])]
    (is (array? (first (slice-views a))))))

(deftest zero-dimension-access
  (testing "Arrays are assumed to be scalars if accessed with zero dimensions"
    (is (equals [1] (mget (byte-array [1]))))
    (is (equals [1] (mget (double-array [1]))))))

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
