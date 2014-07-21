(ns clojure.core.matrix.impl.index
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp]))

(extend-protocol mp/PIndexImplementation
  (Class/forName "[D")
	  (index? [m]
      true) 
	  (index-to-longs [m]
      m)
	  (index-to-ints [m]
      (int-array m))
	  (index-from-longs [m xs]
      xs)
	  (index-from-ints [m xs]
      (long-array xs))
	  (index-coerce [m a]
      (mp/index-to-longs m)))

(extend-protocol mp/PIndexImplementation
  clojure.lang.IPersistentVector
	  (index? [m]
      true) 
	  (index-to-longs [m]
      (long-array m))
	  (index-to-ints [m]
      (int-array m))
	  (index-from-longs [m xs]
      (vec xs))
	  (index-from-ints [m xs]
      (vec xs))
	  (index-coerce [m a]
      (if (== 1 (mp/dimensionality a))
        (vec (mp/index-to-longs a))
        (error "Can't make a 1D index from array of shape " (mp/get-shape a)))))

