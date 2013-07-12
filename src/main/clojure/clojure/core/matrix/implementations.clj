(ns clojure.core.matrix.implementations
  (:use [clojure.core.matrix.utils])
  (:require [clojure.core.matrix.protocols :as mp]))

;; =====================================================
;; Implementation utilities
;;
;; Tools to support the registration / manangement of clojure.core.matrix implementations

;; map of known implementation tags to namespace imports
;; we use this to attempt to load an implementation
(def KNOWN-IMPLEMENTATIONS
  (array-map
   :vectorz 'mikera.vectorz.matrix-api
   :ndarray 'clojure.core.matrix.impl.ndarray
   :persistent-vector 'clojure.core.matrix.impl.persistent-vector
   :persistent-map 'clojure.core.matrix.impl.sparse-map
   :sequence 'clojure.core.matrix.impl.sequence
   :double-array 'clojure.core.matrix.impl.double-array
   :scalar-wrapper 'clojure.core.matrix.impl.wrappers
   :slice-wrapper 'clojure.core.matrix.impl.wrappers
   :nd-wrapper 'clojure.core.matrix.impl.wrappers
   :jblas :TODO
   :clatrix 'clatrix.core
   :parallel-colt :TODO
   :ejml :TODO
   :ujmp :TODO
   :commons-math :TODO))

;; default implementation to use
;; should be included with clojure.core.matrix for easy of use
(def DEFAULT-IMPLEMENTATION :persistent-vector)

;; hashmap of implementation keys to canonical objects
;; objects must implement PImplementation protocol at a minimum
(defonce canonical-objects (atom {}))

(defn get-implementation-key
  "Returns the implementation code for a given object"
  ([m]
    (if (keyword? m)
      m
      (mp/implementation-key m))))

(defn register-implementation
  "Registers a matrix implementation for use. Should be called by all implementations
   when they are loaded."
  ([canonical-object]
  (swap! canonical-objects assoc (mp/implementation-key canonical-object) canonical-object)))

(defn try-load-implementation [k]
  (if-let [ns-sym (KNOWN-IMPLEMENTATIONS k)]
    (try
      (do (require ns-sym) :ok)
      (catch Throwable t nil))))

(defn get-canonical-object
  "Gets the canonical object for a specific implementation"
  ([m]
    (let [k (get-implementation-key m)
          obj (@canonical-objects k)]
      (or obj
          (if (try-load-implementation k) (@canonical-objects k))
          (error "Unable to find implementation: [" k "]")))))
