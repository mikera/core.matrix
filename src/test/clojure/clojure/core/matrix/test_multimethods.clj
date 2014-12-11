(ns clojure.core.matrix.test-multimethods
  (:require [clojure.core.matrix.multimethods :as mm]
            [clojure.test :refer :all]))

(deftest test-hierarchy
  (testing "multimethod hierarchy"
    (is (isa? ::mm/square-matrix ::mm/matrix))
    (is (isa? ::mm/identity-matrix ::mm/matrix))))
