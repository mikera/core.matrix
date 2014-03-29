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
   :ndarray-double 'clojure.core.matrix.impl.ndarray
   :ndarray-float 'clojure.core.matrix.impl.ndarray
   :ndarray-long 'clojure.core.matrix.impl.ndarray
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
   :commons-math 'apache-commons-matrix.core))

;; default implementation to use
;; should be included with clojure.core.matrix for easy of use
(def DEFAULT-IMPLEMENTATION :persistent-vector)

;; hashmap of implementation keys to canonical objects
;; objects must implement PImplementation protocol at a minimum
(defonce canonical-objects (atom {}))

(defn get-implementation-key
  "Returns the implementation code for a given object"
  ([m]
    (cond 
      (keyword? m) m
      (mp/is-scalar? m) nil
      :else (mp/implementation-key m))))

(defn register-implementation
  "Registers a matrix implementation for use. Should be called by all implementations
   when they are loaded."
  ([canonical-object]
    (swap! canonical-objects assoc (mp/implementation-key canonical-object) canonical-object)))

(defn try-load-implementation
  "Attempts to load an implementation for the given keyword.
   Returns nil if not possible, a non-nil value otherwise."
  ([k]
    (if-let [ns-sym (KNOWN-IMPLEMENTATIONS k)]
      (try
        (do 
          (require ns-sym) 
          (if (@canonical-objects k) :ok :warning-implementation-not-registered?))
        (catch Throwable t nil)))))

(defn get-canonical-object
  "Gets the canonical object for a specific implementation. The canonical object is used
   to call implementation-specific protocol functions where required (e.g. creation of new 
   arrays of the correct type for the implementation)"
  ([m]
    (let [k (get-implementation-key m)
          obj (@canonical-objects k)]
      (if k 
        (or obj
           (if (try-load-implementation k) (@canonical-objects k))
           (when-not (keyword? m) m)
           (error "Unable to find implementation: [" k "]"))
        nil))))

(defn construct 
  "Attempts to construct an array according to the type of array m. If not possible,
   returns another array type."
  ([m data]
    (or (mp/construct-matrix m data)
        ;; TODO: use current implementation?
        (mp/coerce-param m data)
        (mp/coerce-param [] data))))