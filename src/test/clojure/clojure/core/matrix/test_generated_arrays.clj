(ns clojure.core.matrix.test-generated-arrays
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.core.matrix.compliance-tester])
  (:require [clojure.core.matrix.generators :as g])
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

(def instance-tests 
  (prop/for-all [a (g/gen-array)] 
                (when (supports-shape? a (shape a))
                  (clojure.core.matrix.compliance-tester/instance-test a))))

(deftest generated-instance-tests
  (is (sc/quick-check 10 instance-tests)))