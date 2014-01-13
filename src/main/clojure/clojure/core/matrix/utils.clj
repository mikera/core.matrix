(ns clojure.core.matrix.utils)

;; Some of these are copies of methods from the library
;;   https://github.com/mikera/clojure-utils
;;
;; duplicated here to avoid an extra dependency

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))

(defmacro error?
  "Returns true if executing body throws an error, false otherwise."
  ([& body]
    `(try
       ~@body
       false
       (catch Throwable t#
         true))))

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))

;; useful TODO macro: facilitates searching for TODO while throwing an error at runtime :-)
(defmacro TODO
  ([] `(error "TODO: not yet implemented")))

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

(defn valid-shape?
  "returns true if the given object is a valid core.matrix array shape."
  ([shape]
    (try
      (and (>= (count shape) 0)
           (every? integer? shape))
      (catch Throwable t false))))

(defn same-shape-object?
  "Returns true if two shapes are the same."
  ([sa sb]
    (cond
      (identical? sa sb) true
      (= sa sb) true
      (not= (count sa) (count sb)) false
      (let [sa (seq sa)
            sb (seq sb)]
        (every? true? (map == sa sb))) true
      :else false)))

(defn xor
  "Returns the logical xor of a set of values, considered as booleans"
  ([] false)
  ([x] (boolean x))
  ([x y] (if x (not y) (boolean y)))
  ([x y & more]
    (loop [p (xor x y) ss (seq more)]
      (if ss
        (recur (if (first ss) (not p) p) (next ss))
        p))))

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
  (^doubles [^doubles arr]
    (java.util.Arrays/copyOf arr (int (alength arr)))))

(defn copy-long-array
  "Returns a copy of a long array"
  (^longs [^longs arr]
    (java.util.Arrays/copyOf arr (int (alength arr)))))

(defn copy-object-array
  "Returns a copy of a long array"
  (^objects [^objects arr]
    (java.util.Arrays/copyOf arr (int (alength arr)))))

(defn long-range
  "Returns a range of longs in a long[] array"
  ([end]
    (let [end (int end)
          ^longs arr (long-array end)]
      (dotimes [i end]
        (aset arr i (long i)))
      arr)))

(defmacro is-object-array? [m]
  `(instance? ~(Class/forName "[Ljava.lang.Object;") ~m))

(defmacro is-long-array? [m]
  `(instance? ~(Class/forName "[J") ~m))

(defmacro is-double-array? [m]
  `(instance? ~(Class/forName "[D") ~m))

(defn to-long-array
  ([data]
    (if (is-long-array? data)
      data
      (long-array data))))

(defn long-array-of
  "Creates a long array with the specified values."
  ([] (long-array 0))
  ([a]
    (let [arr (long-array 1)]
      (aset arr 0 (long a))
      arr))
  ([a b]
    (let [arr (long-array 2)]
      (aset arr 0 (long a))
      (aset arr 1 (long b))
      arr))
  ([a b & more]
    (let [arr (long-array (+ 2 (count more)))]
      (aset arr 0 (long a))
      (aset arr 1 (long b))
      (doseq-indexed [x more i] (aset arr (+ 2 i) (long x)))
      arr)))

(defn object-array-of
  "Creates a long array with the specified values."
  ([] (object-array 0))
  ([a]
    (let [arr (object-array 1)]
      (aset arr 0 a)
      arr))
  ([a b]
    (let [arr (object-array 2)]
      (aset arr 0 a)
      (aset arr 1 b)
      arr))
  ([a b & more]
    (let [arr (object-array (+ 2 (count more)))]
      (aset arr 0 a)
      (aset arr 1 b)
      (doseq-indexed [x more i] (aset arr (+ 2 i) x))
      arr)))

(defn base-index-seq-for-shape [sh]
  "Returns a sequence of all possible index vectors for a given shape, in row-major order"
  (let [gen (fn gen [prefix rem]
              (if rem
                (let [nrem (next rem)]
                  (mapcat #(gen (conj prefix %) nrem) (range (first rem))))
                (list prefix)))]
    (gen [] (seq sh))))

(defn- broadcast-shape*
  "Returns the smallest shape that both shapes a and b can broadcast to, or nil if the the shapes
   are not compatible."
  ([a b]
    (cond
      (empty? a) (or b '())
      (empty? b) a
      (== 1 (first a)) (broadcast-shape* (first b) (next a) (next b))
      (== 1 (first b)) (broadcast-shape* (first a) (next a) (next b))
      (== (first a) (first b)) (broadcast-shape* (first a) (next a) (next b))
      :else nil))
  ([prefix a b]
    (if (or a b)
      (let [r (broadcast-shape* a b)]
        (if r (cons prefix r) nil))
      (cons prefix nil))))

(defn broadcast-shape
  "Returns the smallest compatible shape that a set of shapes can all broadcast to.
   Returns nil if this is not possible (i.e. the shapes are incompatible).
   Returns an empty list if both shape sequences are empty (i.e. represent scalars)"
  ([a] a)
  ([a b]
    (let [a (seq (reverse a))
          b (seq (reverse b))
          r (broadcast-shape* a b)]
      (if r (reverse r) nil))))

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
         new-xs# (java.util.Arrays/copyOf ~xs (int (dec n#)))]
     (c-for [j# (int ~i) (< j# (dec n#)) (inc j#)]
       (aset new-xs# (int j#) (aget ~xs (int (inc j#)))))
     new-xs#))

(defmacro areverse [xs]
  `(let [n# (alength ~xs)
         new-xs# (java.util.Arrays/copyOf ~xs (int n#))]
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

(defn protocol?
  "Returns true if an argument is a protocol'"
  [p]
  (and (map? p)
       (:on-interface p)
       (.isInterface ^Class (:on-interface p))))

(defn enhance-protocol-kv
  "Transform MapEntry to just map with some additional fields"
  [[name p]]
  (let [m (->> @p :var meta)]
    (assoc @p :line (:line m) :file (:file m) :name name)))

(defn extract-protocols
  "Extracts protocol info from clojure.core.matrix.protocols"
  []
  (->> (ns-publics 'clojure.core.matrix.protocols)
       (filter (comp protocol? deref val))
       (map enhance-protocol-kv)
       (sort-by :line)))
