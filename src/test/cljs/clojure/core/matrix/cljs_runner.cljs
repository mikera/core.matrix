(ns clojure.core.matrix.cljs-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [clojure.core.matrix.test-api]
            [clojure.core.matrix.test-dataset]))

(doo-tests 'clojure.core.matrix.test-dataset)
(doo-tests 'clojure.core.matrix.test-api)
