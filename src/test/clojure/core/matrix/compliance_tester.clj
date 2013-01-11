(ns core.matrix.compliance-tester
  (:use core.matrix)
  (:use clojure.test)
  (:use core.matrix.utils))

;; ====================================
;; COMPLIANCE TESTING
;;
;; test suite that implementations can call to test
;; adherence to core.matrix API contracts


(defn compliance-test 
  "Runs the compliance test suite on a given matrix implementation"
  [m]
  (is (= 1 1)))