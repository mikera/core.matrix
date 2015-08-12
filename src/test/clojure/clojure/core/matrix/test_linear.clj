(ns clojure.core.matrix.test-linear
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.linear :refer :all]
            [clojure.core.matrix.operators :refer :all]))

(defn- approx=
  [a b]
  (equals a b 0.0001))

(deftest test-condition-number
  ; square matrices
  (testing "Square matrices" 
    (is (approx= (condition-number (matrix :vectorz 
                                           [[8 1 6]
                                            [3 5 7]
                                            [4 9 2]])) 4.3301))
     
    (is (approx= (condition-number (matrix  :vectorz
                                           [[0.238868   0.341547   0.057347]
                                            [0.758487   0.540194   0.483975]
                                            [0.238713   0.518180   0.766142]]))
                 8.1834)))

  ; non square matrices 
  (testing "non square matrices"
    (is (approx= (condition-number (matrix :vectorz
                                           [[1 2 3 4 5]
                                            [9 8 7 6 5]]))
                 4.1427))))
