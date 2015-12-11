(ns test.misc.test-load
  (:require [clojure.core.matrix.macros :refer [error]]))

(defn foo []
  (doall
   (map deref (for [i (range 10)]
                (future
                  (require 'test.misc.loading-test)
                  (if (not (deref (resolve 'test.misc.loading-test/loaded)))
                    (error "Not loaded!")
                    :OK))))))
