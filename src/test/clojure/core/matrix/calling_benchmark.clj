(ns core.matrix.calling-benchmark
  (:require [criterium.core :as c]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

;; benchmark for a few different calling conventions

(def arr (long-array 1))

(definterface IInterface
  (interfaceCall [i])
  (interfaceCallPrim [^long i]))

(defprotocol POperation
  (proto-call [m i]))

(defprotocol PExtendedOperation
  (extended-call [m i]))

(deftype MyType []
  POperation
    (proto-call [m i]
      (aset ^longs arr 0 (long i)))
  IInterface
    (interfaceCall [m i]
      (aset ^longs arr 0 (long i)))
    (interfaceCallPrim [m ^long i]
      (aset ^longs arr 0 i)))

(extend-protocol PExtendedOperation
  MyType
    (extended-call [m i]
      (aset ^longs arr 0 (long i)))) 

(def my-type (MyType.))

(defn prim-call [^long x]
  (aset ^longs arr 0 x) nil)

(defn boxed-call [^Long x]
  (aset ^longs arr 0 (long x)) nil)

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
;; fast - boxing adds overhead

(c/quick-bench (dotimes [i 1000] (aset arr 0 i)))
;; 9002.24 us
;; reflection is very expensive - dwarfs method calling and boxing costs


(c/quick-bench (dotimes [i 1000] (proto-call my-type i)))
;; 7.95 us
;; same as regular boxed method call

(c/quick-bench (dotimes [i 1000] (extended-call my-type i)))
;; 13.81 us
;; more expensive than regular protocol call

(c/quick-bench (dotimes [i 1000] (.interfaceCall ^IInterface my-type i)))
;; 6.93 us
;; pretty fast

(c/quick-bench (dotimes [i 1000] (.interfaceCallPrim ^IInterface my-type i)))
;; 2.28 us
;; very fast!

)
