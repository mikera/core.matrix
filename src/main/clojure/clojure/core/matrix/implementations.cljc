(ns clojure.core.matrix.implementations
  "Namespace for management of core.matrix implementations. Users should avoid using these
   functions directly as they are intended for library and tool writers."
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.macros #?(:clj :refer :cljs :refer-macros) [TODO error]]))

;; =====================================================
;; Implementation utilities
;;
;; Tools to support the registration / manangement of clojure.core.matrix implementations

(def KNOWN-IMPLEMENTATIONS
  "A map of known core.matrix implementation namespaces.

   core.matrix will attempt to load these namespaces when an array of the specified
   keyword type is requested."
  (array-map
   :vectorz 'mikera.vectorz.matrix-api
   :vectorz-opencl 'mikera.vectorz.opencl-api
   :neanderthal 'uncomplicate.neanderthal.impl.matrix-api
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
   :nd4j 'nd4clj.kiw
   :ujmp :TODO
   :weka 'clj-ml.matrix-api
   :commons-math 'apache-commons-matrix.core
   :mtj 'cav.mtj.core.matrix
   :aljabr 'thinktopic.aljabr.core))

(def DEFAULT-IMPLEMENTATION
  "The default implementation used in core.matrix. Currently set to `:persistent-vector` for maximum
   compatibility with regular Clojure code."
  :persistent-vector)


(def ^:dynamic *matrix-implementation*
  "A dynamic var specifying the current core.matrix implementation in use.

   May be re-bound to temporarily use a different core.matrix implementation."
  DEFAULT-IMPLEMENTATION)

(def ^:dynamic *numeric-implementation*
  "A dynamic var specifying the current core.matrix numeric implementation in use.

   May be re-bound to temporarily use a different core.matrix implementation."
  :ndarray-double)

(defonce
  ^{:doc "A dynamic var supporting debugging option for core.matrix implementers.

   Currently supported values:
     :print-registrations  - print when core.matrix implementations are registered
     :reload-namespaces  - require :reload implementation namespaces when setting the current implementation"
    :dynamic true}
  *debug-options* {:print-registrations false
                   :reload-namespaces false})

(defonce
  ^{:doc "An atom holding a map of canonical objects for each loaded core.matrix implementation.

   Canonical objects may be used to invoke protocol methods on an instance of the correct
   type to get implementation-specific behaviour. Canonical objects are required to support
   all mandatory core.matrix protocols."}
  canonical-objects (atom {}))

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
    (when-not (keyword? key) (error "Implementation key must be a Clojure keyword but got: "
                                    #?(:clj (class key)
                                       :cljs (type key))))
    (when (:print-registrations *debug-options*)
      (println (str "Registering core.matrix implementation [" key "] with canonical object ["
                    #?(:clj (class canonical-object)
                       :cljs (type canonical-object)) "]")))
    (swap! canonical-objects assoc key canonical-object)))

(defn- try-load-implementation
  "Attempts to load an implementation for the given keyword.
   Returns nil if not possible, a non-nil matrix value of the correct implementation otherwise."
  ([k]
   #?(:clj
       (or (@canonical-objects k)
           (if-let [ns-sym (KNOWN-IMPLEMENTATIONS k)]
             (try
               (do
                 (when (:print-registrations *debug-options*)
                   (println (str "Loading core.matrix implementation [" k "] in ns: " ns-sym)))
                 (if (:reload-namespaces *debug-options*)
                   (require :reload ns-sym)
                   (require ns-sym))
                 (@canonical-objects k))
               (catch Throwable t
                 (println "Error loading core.matrix implementation: " ns-sym)
                 (println t)))))
       :cljs (println "INFO: No dynamic loading of implementations in Clojurescript.\nYou must require an implementation explicitly in a namespace, for example thinktopic.aljabr.core"))))

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
    - An existing instance from the implementation

   Throws an exception if the implementation cannot be loaded.

   This is used primarily for functions that construct new matrices, i.e. it determines the
   implementation used for expressions like: (matrix [[1 2] [3 4]])"
  ([m]
    (when (keyword? m) 
      (or (try-load-implementation m) (error "Unable to load matrix implementation: " m)))
    #?(:clj (alter-var-root (var *matrix-implementation*)
                    (fn [_] (get-implementation-key m)))
       :cljs (set! *matrix-implementation* (get-implementation-key m)))))
