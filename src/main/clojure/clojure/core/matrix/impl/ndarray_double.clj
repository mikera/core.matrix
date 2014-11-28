(ns clojure.core.matrix.impl.ndarray-double
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:use clojure.core.matrix.impl.ndarray-macro)
  (:require [clojure.core.matrix.impl.default])
  (:require [clojure.core.matrix.impl.ndarray-magic :as magic])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?])

  (:use clojure.core.matrix.impl.ndarray))

(magic/spit-code :double)
