(ns clojure.core.matrix.test-multimethods
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require clojure.core.matrix.impl.persistent-vector))

(deftest test-hierarchy
  (testing "multimethod hierarchy"
    (is (isa? ::mm/square-matrix ::mm/matrix))
    (is (isa? ::mm/identity-matrix ::mm/matrix))))
