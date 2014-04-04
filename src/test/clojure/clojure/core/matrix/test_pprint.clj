(ns clojure.core.matrix.test-pprint
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.impl.pprint :as pp]))

(deftest test-basic-pprint
  (is (= "[1.000 2.000]" (pp/pm [1 2]))))