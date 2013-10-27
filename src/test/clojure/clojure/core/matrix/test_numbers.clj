(ns clojure.core.matrix.test-numbers
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:refer-clojure :exclude [vector?])
  (:use clojure.test))

(deftest test-scalar-properties
  (is (not (array? 1)))
  (is (nil? (shape 1))))

(deftest test-arithmentic
  (is (== 2 (add 1 1)))
  (is (== 2 (sub 5 3)))
  (is (== 12 (mul 3 4))))

(deftest test-mutable-failure
  (is (error? (add! 1 1)))
  (is (error? (sub! 1 1)))
  (is (error? (mul! 1 1))))