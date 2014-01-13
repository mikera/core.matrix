(ns clojure.core.matrix.test-utils
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require clojure.core.matrix.examples)
  (:require clojure.core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

;; Tests for clojure.core.matrix.utils functions and macros

(deftest test-long-array
  (is (= (type (long-array 0)) (type (long-array nil))))
  (is (= (count (long-array 0)) (count (long-array nil)))))
