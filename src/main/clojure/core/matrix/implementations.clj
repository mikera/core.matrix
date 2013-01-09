(ns core.matrix.implementations)

;; map of known implementation tags to namespace imports

(def DEFAULT-IMPLEMENTATION :perstistent-vector)

(def KNOWN-IMPLEMENTATIONS
  {:vectorz 'mikera.vectorz.matrix-api
   :ndarray 'core.matrix.impl.ndarray
   :persistent-vector 'core.matrix.impl.persistent-vector
   :jblas :TODO
   :clatrix :TODO
   :parallel-colt :TODO
   :ejml :TODO
   :ujmp :TODO
   :commons-math :TODO})