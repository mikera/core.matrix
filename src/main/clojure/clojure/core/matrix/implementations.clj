(ns clojure.core.matrix.implementations
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.utils :refer [error]]))

;; =====================================================
;; Implementation utilities
;;
;; Tools to support the registration / manangement of clojure.core.matrix implementations

;; map of known implementation tags to namespace imports
;; we use this to attempt to load an implementation
(def KNOWN-IMPLEMENTATIONS
  (array-map
   :vectorz 'mikera.vectorz.matrix-api
   :clojure 'clojure.core.matrix.impl.clojure
   :ndarray 'clojure.core.matrix.impl.ndarray-object
   :ndarray-double 'clojure.core.matrix.impl.ndarray-double
   :ndarray-float 'clojure.core.matrix.impl.ndarray
   :ndarray-long 'clojure.core.matrix.impl.ndarray
   :persistent-vector 'clojure.core.matrix.impl.persistent-vector
   :persistent-map 'clojure.core.matrix.impl.sparse-map
   :sequence 'clojure.core.matrix.impl.sequence
   :double-array 'clojure.core.matrix.impl.double-array
   :scalar-wrapper 'clojure.core.matrix.impl.wrappers
   :slice-wrapper 'clojure.core.matrix.impl.wrappers
   :nd-wrapper 'clojure.core.matrix.impl.wrappers
   :dataset 'clojure.core.matrix.impl.dataset
   :jblas :TODO
   :clatrix 'clatrix.core
   :parallel-colt :TODO
   :ejml :TODO
   :ujmp :TODO
   :commons-math 'apache-commons-matrix.core
   :mtj 'cav.mtj.core.matrix))

;; default implementation to use
;; should be included with clojure.core.matrix for easy of use
(def DEFAULT-IMPLEMENTATION :persistent-vector)

;; current implementation in use
(def ^:dynamic *matrix-implementation* DEFAULT-IMPLEMENTATION)

(def ^:dynamic *debug-options* {:print-registrations false})

;; hashmap of implementation keys to canonical objects
;; objects must implement PImplementation protocol at a minimum
(defonce canonical-objects (atom {}))

(defn get-implementation-key
  "Returns the implementation keyword  for a given object"
  ([m]
    (cond
      (keyword? m) m
      (mp/is-scalar? m) nil
      :else (mp/implementation-key m))))

(defn register-implementation
  "Registers a matrix implementation for use. Should be called by all implementations
   when they are loaded, once for each implementation keyword registered. Safe to call multiple times."
  ([canonical-object]
    (register-implementation (mp/implementation-key canonical-object) canonical-object))
  ([key canonical-object]
    (when-not (keyword? key) (error "Implementation key must be a Clojure keyword but got: " (class key))) 
    (when (:print-registrations *debug-options*)
      (println (str "Registering core.matrix implementation [" key "] with canonical object [" (class canonical-object) "]")))
    (swap! canonical-objects assoc key canonical-object)))

(defn- try-load-implementation
  "Attempts to load an implementation for the given keyword.
   Returns nil if not possible, a non-nil matrix value of the correct implementation otherwise."
  ([k]
    (or
      (@canonical-objects k)
      (if-let [ns-sym (KNOWN-IMPLEMENTATIONS k)]
       (try
         (do
           (require ns-sym)
           (@canonical-objects k))
         (catch Throwable t nil))))))

(defn load-implementation 
  "Attempts to load the implementation for a given keyword or matrix object.
   Returns nil if not possible, a non-nil matrix value of the correct implementation otherwise."
  ([korm] 
    (if (keyword? korm)
      (try-load-implementation korm)
      (try-load-implementation (mp/implementation-key korm)))))

(defn get-canonical-object
  "Gets the canonical object for a specific implementation. The canonical object is used
   to call implementation-specific protocol functions where required (e.g. creation of new
   arrays of the correct type for the implementation).

   Returns nil if the implementation cannot be found."
  ([]
    (get-canonical-object *matrix-implementation*))
  ([m]
    (let [k (get-implementation-key m)
          obj (@canonical-objects k)]
      (if k
        (or obj
           (if (try-load-implementation k) (@canonical-objects k))
           (when-not (keyword? m) m)
           nil)
        nil))))

(defn get-canonical-object-or-throw
  "Like get-canonical-object, except it throws an exception if the implementation cannot be found"
  ([mk]
    (or (get-canonical-object mk) (error "Cannot find implementation for " mk))))

(defn construct
  "Attempts to construct an array according to the type of array m. If not possible,
   returns another array type."
  ([m data]
    (or (mp/construct-matrix m data)
        ;; TODO: use current implementation?
        (mp/coerce-param m data)
        (mp/coerce-param [] data))))

(defn set-current-implementation
  "Sets the currently active core.matrix implementation. 

   Parameter may be 
    - A known keyword for the implementation e.g. :vectorz

   This is used primarily for functions that construct new matrices, i.e. it determines the
   implementation used for expressions like: (matrix [[1 2] [3 4]])"
  ([m]
    (when (keyword? m) (try-load-implementation m))
    (alter-var-root (var *matrix-implementation*)
                    (fn [_] (get-implementation-key m)))))


