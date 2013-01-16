(ns core.matrix.calling-benchmark
  (:require [criterium.core :as c]))

;; benchmark for a few different calling conventions

(set! *unchecked-math* true)
;; (set! *warn-on-reflection* true)

;; an array to mutate: gives methods something to do with a side effect
(def arr (long-array 1))

;; Java interface
(definterface IInterface
  (interfaceCall [i])
  (interfaceCallPrim [^long i]))

;; Protocol
(defprotocol POperation
  (proto-call [m i]))

;; Protocol for extending later
(defprotocol PExtendedOperation
  (extended-call [m i]))

;; Clojure detype implementing POperation and IInterface
(deftype MyType []
  POperation
    (proto-call [m i]
      (aset ^longs arr 0 (long i)))
  IInterface
    (interfaceCall [m i]
      (aset ^longs arr 0 (long i)))
    (interfaceCallPrim [m ^long i]
      (aset ^longs arr 0 i)))

;; extend PExtendedOperation to MyType *after* it is defined
;; so we can determine extra overhead of extending
(extend-protocol PExtendedOperation
  MyType
    (extended-call [m i]
      (aset ^longs arr 0 (long i)))) 

;; an instance of MyType
(def ^MyType my-type (MyType.))

(defn prim-call [^long x]
  (aset ^longs arr 0 x) nil)

(defn boxed-call [^Long x]
  (aset ^longs arr 0 (long x)) nil)

(defmulti class-multimethod (fn [m i] (class m)))
(defmethod class-multimethod MyType [m i] (aset ^longs arr 0 (long i)))
(defmethod class-multimethod :default [m i] (throw (IllegalArgumentException.)))

(defmulti double-multimethod (fn [m i] [(class m) (class i)]))
(defmethod double-multimethod [MyType Long] [m i] (aset ^longs arr 0 (long i)))
(defmethod double-multimethod :default [m i] (throw (IllegalArgumentException.)))

;; =============================================================
;; Benchmark code follows

(defn all-benchmarks []
  
(c/quick-bench (dotimes [i 1000] (aset ^longs arr 0 i)))
;; 1.23 us
;; extremely fast!

(c/quick-bench (dotimes [i 1000] (prim-call i)))
;; 1.84 us
;; very fast!

(c/quick-bench (dotimes [i 1000] (boxed-call i)))
;; 7.95 us
;; fast - boxing adds moderate overhead

(c/quick-bench (dotimes [i 1000] (aset arr 0 i)))
;; 9002.24 us
;; reflection is very expensive - dwarfs method calling and boxing costs


(c/quick-bench (dotimes [i 1000] (proto-call my-type i)))
;; 7.95 us
;; fast - similar to regular boxed method call

(c/quick-bench (dotimes [i 1000] (extended-call my-type i)))
;; 13.81 us
;; quite fast - more expensive than regular protocol call

(c/quick-bench (dotimes [i 1000] (.interfaceCall ^IInterface my-type i)))
;; 6.93 us
;; fast

(c/quick-bench (dotimes [i 1000] (.interfaceCallPrim ^IInterface my-type i)))
;; 1.89 us
;; very fast!

(c/quick-bench (dotimes [i 1000] (.interfaceCallPrim ^MyType my-type i)))
;; 1.86 us
;; very fast!

(c/quick-bench (dotimes [i 1000] (class-multimethod my-type i)))
;; 88.98 us
;; slower 

(c/quick-bench (dotimes [i 1000] (double-multimethod my-type i)))
;; 231.00 us
;; much slower! still better than reflection though....
)
