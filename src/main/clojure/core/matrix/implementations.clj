(ns core.matrix.implementations)


;; map of known implementation tags to namespace imports
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

;; default implementation to use
;; should be included with core.matrix for easy of use
(def DEFAULT-IMPLEMENTATION :perstistent-vector)
