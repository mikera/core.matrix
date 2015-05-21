(ns clojure.core.matrix.test-generated-arrays
  (:require [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix.generators :as g]
            [clojure.core.matrix :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer (defspec)]))


(defspec instance-tests
  (let [array-generator (g/gen-array (g/gen-shape) g/gen-double)]
    (prop/for-all [a array-generator]
      (when (supports-shape? a (shape a))
        (compliance/instance-test a)))))
;
;(deftest generated-instance-tests
;  (is (sc/quick-check 20 instance-tests :seed 2964321771959749102)))
