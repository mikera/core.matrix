(ns clojure.core.matrix.utils)

;; these are copies of methods from the library
;;   https://github.com/mikera/clojure-utils
;;
;; duplicated here to avoid an extra dependency

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))

;; useful TODO macro facilitates searching for TODO while throwing an error at runtime :-)
(defmacro TODO 
  ([]
    `(error "TODO: not yet implemented"))) 


(defmacro doseq-indexed 
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
  `(loop [vals# (seq ~values) 
          ~index-sym (long 0)]
     (if vals#
       (let [~val-sym (first vals#)]
             ~@code
             (recur (next vals#) (inc ~index-sym)))
       nil))))

(defn copy-double-array 
  "Returns a copy of a double array"
  ([^doubles arr]
    (java.util.Arrays/copyOf arr (int (alength arr))))) 
