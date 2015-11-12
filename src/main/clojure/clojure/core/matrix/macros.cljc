(ns clojure.core.matrix.macros
  "Namespace for core.matrix macros. Keeping them separate allows us to do conditional
  macros that can handle the differences between Clojure and Clojurescript."
  #?(:clj (:import [java.util Arrays])))

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (#?(:clj RuntimeException.
                :cljs js/Error.)
                     (str ~@vals)))))

(defmacro error?
  "Returns true if executing body throws an error, false otherwise."
  ([& body]
    `(try
       ~@body
       false
       (catch #?(:clj Throwable :cljs js/Error) t#
         true))))

;; useful TODO macro: facilitates searching for TODO while throwing an error at runtime :-)
(defmacro TODO
  ([] `(error "TODO: not yet implemented"))
  ([& vals] `(error "TODO: " ~@vals)))

(defmacro iae
  "Throws IllegalArgumentException with provided string"
  [exception-str]
  `(throw (IllegalArgumentException. ~exception-str)))

(defmacro iae-when-not
  "Throws an IllegalArgumentException when the predicate is not satisfied"
  [pred? exception-str]
  `(when-not ~pred?
     (iae ~exception-str)))

(defmacro java-array? [m]
  `(.isArray (.getClass ~m)))

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

(defmacro is-object-array? [m]
  #?(:clj `(instance? ~(Class/forName "[Ljava.lang.Object;") ~m)
     :cljs `(= js/Array (type ~m))))

(defmacro is-long-array? [m]
  #?(:clj `(instance? ~(Class/forName "[J") ~m)
     :cljs `(= js/Array (type ~m))))

(defmacro is-double-array? [m]
  #?(:clj `(instance? ~(Class/forName "[D") ~m)
     :cljs `(= js/Array (type ~m))))

(defmacro c-for
  "C-like loop with nested loops support"
  [loops & body]
  (letfn [(c-for-rec [loops body-stmts]
            (if (seq loops)
              (let [[var init check next] (take 4 loops)]
                `((loop [~var ~init]
                     (when ~check
                       ~@(c-for-rec (nthrest loops 4) body-stmts)
                       (recur ~next)))))
              body-stmts))]
    `(do ~@(c-for-rec loops body) nil)))

(defmacro abutnth [i xs]
  `(let [n# (alength ~xs)
         length# (int (dec n#))
         new-xs# #?(:clj (Arrays/copyOf ~xs length#)
                    :cljs (.slice ~xs 0 length#))]
     (c-for [j# (int ~i) (< j# (dec n#)) (inc j#)]
       (aset new-xs# (int j#) (aget ~xs (int (inc j#)))))
     new-xs#))

(defmacro areverse [xs]
  `(let [n# (alength ~xs)
         new-xs# #?(:clj (Arrays/copyOf ~xs (int n#))
                    :cljs (.slice ~xs 0 n#))]
     (c-for [i# (int 0) (< i# (quot n# 2)) (inc i#)]
       (let [j# (- (- n# 1) i#)
             t# (aget new-xs# j#)]
         (aset new-xs# j# (aget new-xs# i#))
         (aset new-xs# i# t#)))
     new-xs#))

(defmacro scalar-coerce
  "Macro to coerce to scalar value with an efficient dispatch sequence"
  ([x]
  `(let [x# ~x]
     (cond
       (number? x#) x#
       :else (clojure.core.matrix.protocols/get-0d x#)))))


