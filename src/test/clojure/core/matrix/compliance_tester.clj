(ns core.matrix.compliance-tester
  (:use core.matrix)
  (:use clojure.test)
  (:require [core.matrix.implementations :as imp])
  (:use core.matrix.utils))

;; ====================================
;; COMPLIANCE TESTING
;;
;; test suite that implementations can call to test
;; adherence to core.matrix API contracts

(defn test-implementation-key
  [m]
  (testing "Implementation keyword"
    (is (keyword? (imp/get-implementation-key m)))
    (is (= (imp/get-implementation-key m) (imp/get-implementation-key (imp/get-canonical-object m))))))

(defn compliance-test 
  "Runs the compliance test suite on a given matrix implementation"
  [m]
  (test-implementation-key m))