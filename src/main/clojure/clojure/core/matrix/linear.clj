(ns clojure.core.matrix.linear
  (:require [clojure.core.matrix.impl default])
  (:require [clojure.core.matrix.protocols :as mp]))


(defn qr
  ([m options] (mp/qr m options))
  ([m] (mp/qr m {:return [:Q :R]})))
