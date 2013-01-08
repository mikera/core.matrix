(ns core.matrix.test-multimethods
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.multimethods :as mm])
  (:require core.matrix.impl.persistent-vector))

(deftest test-hierarchy
  (testing "multimethod hierarchy"
    (is (isa? ::mm/square-matrix ::mm/matrix))
    (is (isa? ::mm/identity-matrix ::mm/matrix))))
