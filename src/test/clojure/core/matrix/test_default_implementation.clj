(ns core.matrix.test-default-implementation
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.operators :as op])
  (:require core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

(deftest test-indexed-access
  (testing "indexed access to java.util.List"
    (let [al (java.util.ArrayList.)]
      (.add al 1.0)
      (.add al 2.0)
      (.add al 3.0)
      (is (= [1.0 2.0 3.0] (coerce [] al)))
      (is (== 1.0 (mget al 0))))))
