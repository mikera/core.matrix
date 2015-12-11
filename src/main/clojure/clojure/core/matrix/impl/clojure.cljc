(ns clojure.core.matrix.impl.clojure
  (:require
    [clojure.core.matrix.implementations :as imp]
    [clojure.core.matrix.impl.persistent-vector]
    [clojure.core.matrix.impl.sequence]))

;; standard core.matrix implementation for main clojure types

#?(:clj
    (imp/register-implementation :clojure [2/7]))
