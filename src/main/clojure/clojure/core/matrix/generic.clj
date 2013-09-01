(ns clojure.core.matrix.generic
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp]))

;; Placeholder namespace for generic versions of core.matrix algorithms

;; generic element accessor functions
;; TODO: revisit these names?

(defn default-value
  "Returns the default value for the given array. Will normally be either nil or zero."
  ([impl]
    (mp/get-1d (mp/new-matrix-nd (imp/get-canonical-object impl) [1]) 0)))

(defn zero
  "Returns the standard 'zero' scalar value for the given array / implementation"
  ([impl] (TODO)))

(defn one
  "Returns the standard 'one' scalar value for the given array / implementation"
  ([impl] (TODO)))

(defn two
  "Returns the standard 'two' scalar value for the given array / implementation"
  ([impl] 
    (let [o (one impl)]
      (mp/get-0d (mp/matrix-add o o)))))

;; TODO: better naming for these?
(defn add-fn
  "Returns the standard 'add' function for the given array / implementation."
  ([impl] (TODO)))

(defn mul-fn
  "Returns the standard 'multiply' function for the given array / implementation."
  ([impl] (TODO)))