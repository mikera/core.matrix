(ns clojure.core.matrix.test-pprint
  (:require [clojure.core.matrix.impl.pprint :as pp]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]))

(deftest test-basic-pprint
  (is (= "[1.000 2.000]" (pp/pm [1.0 2.0]))))

(deftest test-vectorz-pm
  (is (pp/pm (array :vectorz [1 2])))
  (is (pp/pm (array :vectorz [[1 2] [3 4]])))
  (is (pp/pm (array :vectorz [[[1 2] [3 4]] [[5 6] [7 8]]]))))

(deftest test-non-numeric
  (is (= "[:a foo bar  ]" (pp/pm [:a "foo" 'bar \space]))))
