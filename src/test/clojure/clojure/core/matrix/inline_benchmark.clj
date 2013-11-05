(ns clojure.core.matrix.inline-benchmark
  (:require [criterium.core :as c])
  (:import [clojure.core.matrix ClassPair]))

;; benchmark for inline usage

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn add-fn
  ([x y]
    (+ x y)))

(defn add-prim 
  (^long [^long x ^long y]
    (+ x y)))

(defn add-inline 
  {:inline (fn ([x y] `(+ ~x ~y)))}
  ([x y]
    (+ x y)))

(defn length-fn
  ([x] (.length x)))

(defn length-hinted
  ([^String x] (.length x)))

(defn length-inline 
  {:inline (fn ([x] `(.length ~x)))}
  ([x]
    (.length x)))

(comment
  ;; normal fn - 23ns per call
  (c/quick-bench (dotimes [i 1000] (add-fn i 10)))

  ;; primitive fn - 1.25 ns per call
  (c/quick-bench (dotimes [i 1000] (add-prim i 10)))
  
  ;; inline fn - 1.2 ns per call
  (c/quick-bench (dotimes [i 1000] (add-inline i 10)))
  
  
  ;; fn with relection - 2060 ns per call
  (c/quick-bench (dotimes [i 1000] (length-fn "foo")))

  ;; fn with type hints - 0.9 ns per call
  (c/quick-bench (dotimes [i 1000] (length-hinted "foo")))
  
  ;; inline fn - 0.6 ns per call
  (c/quick-bench (dotimes [i 1000] (length-inline "foo")))
 )
