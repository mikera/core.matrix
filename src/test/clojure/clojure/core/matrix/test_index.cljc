(ns clojure.core.matrix.test-index
  (:require [clojure.core.matrix :as m])
  #?(:clj (:require [clojure.core.matrix.macros-cljs :refer [error?]]
                    [clojure.test :refer [deftest is testing]])
    :cljs (:require-macros
            [clojure.core.matrix.macros-cljs :refer [error?]]
            [cljs.test :refer [deftest is testing]])))

(deftest test-int-index
  (let [xs (int-array [1 2 3])]
    (testing "Index identity"
	    (is (identical? xs (m/index xs xs)))
	    (is (m/index? xs)))))

(deftest test-long-index
  (let [xs (long-array [1 2 3])]
    (testing "Index identity"
	    (is (identical? xs (m/index xs xs)))
	    (is (m/index? xs)))))

(deftest test-vector-index
  (let [xs [1 2 3]]
    (testing "Index identity"
	    ;; (is (identical? xs (index xs xs)))
	    (is (m/index? xs)))))

(deftest test-index-coercions
  (is (m/index? (m/index '(1 2 3)))))

