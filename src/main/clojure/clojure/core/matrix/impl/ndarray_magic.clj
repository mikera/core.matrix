(ns clojure.core.matrix.impl.ndarray-magic
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)

(defn init
  "Initializes specialization map (see `ndarray` namespace for details)"
  [types]
  (def type-table-magic types)
  (def deftypes-magic (atom {}))
  (def defns-magic (atom {})))

(defn form-replaces
  "Takes specialization map and returns a map with symbols with hashes as
   keys"
  [extra-parts type]
  (->> (merge (type-table-magic type) (extra-parts type))
       (map (juxt #(-> % key name (str "#") symbol) val))
       (into {})))

(defn add-fn-suffix
  "Takes a type and symbol that is a name of the function and adds a suffix
   to it"
  [type sym]
  (if-let [suffix (->> type type-table-magic :fn-suffix)]
    (->> suffix name (str sym "-") symbol)
    (symbol sym)))

(defn handle-symbol
  "Takes a symbol and possibly replace it, saving meta and making all
   substitutions to it"
  [type replaces sym]
  (let [new-sym (or (replaces sym)
                    (if (.endsWith (name sym) "#t")
                      (->> sym name (drop-last 2) (apply str)
                           (add-fn-suffix type))
                      sym))]
    (if-let [m (meta sym)]
      (with-meta new-sym
        (w/postwalk-replace replaces m))
      new-sym)))

(defn special-macro?
  "This predicate returns true if provided symbol is a name of 'special'
   macro that should be expanded during the specialization. This is needed
   because usually macroexpansion is done from the outside, so if 'magic'
   variables are used in macros that occur in specialized code, they will
   need to be explicitly expanded before specialization. Therefore all such
   macros should be listed here"
  [macro]
  (let [names #{"loop-over"
                "loop-over-0d"
                "loop-over-0d-internal"
                "loop-over-1d"
                "loop-over-1d-internal"
                "loop-over-2d"
                "loop-over-2d-internal"
                "loop-over-nd"
                "loop-over-nd-internal"
                "expose-ndarrays"
                "fold-over"
                "fold-over-0d-internal"
                "fold-over-1d-internal"
                "fold-over-2d-internal"
                "fold-over-nd-internal"}]
    (contains? names (name macro))))

(defn handle-forms
  "Specializing forms recursively"
  [type replaces form]
  (w/prewalk
   (fn [x]
     (if (symbol? x) (handle-symbol type replaces x)
         (if (and (seq? x) (symbol? (first x))
                  (special-macro? (first x)))
           (handle-forms type replaces (macroexpand-1 x))
           x)))
   form))

(defn handle-defn-form
  "Special treating for defn -- we need to define specialized versions of
   provided function with different names"
  [t replaces [_ fn-name & _ :as form]]
  (let [new-fn-name (add-fn-suffix t fn-name)
        new-replaces (assoc replaces fn-name new-fn-name)]
    (handle-forms t new-replaces [new-fn-name form])))

(defmacro with-magic
  "Macro for collecting forms for specialization. See `ndarray` namespace for
   details"
  ([types form] `(with-magic ~types {} ~form))
  ([types extra-parts form]
     (doseq [t types
             :let [replaces (form-replaces extra-parts t)]]
       (when (type-table-magic t) ;; skip if not defined in type table
         (case (first form)
         deftype (swap! deftypes-magic assoc t
                        (handle-forms t replaces form))
         defn (swap! defns-magic conj
                     (handle-defn-form t replaces form))
         :else (throw (IllegalArgumentException.
                       "only deftype and defn are supported in with-magic")))))
     :ok))

(defmacro extend-types
  "Collects forms of methods that will be attached to deftype"
  [types & forms]
  (let [[extra-parts forms] (if (map? (first forms))
                              [(first forms) (rest forms)]
                              [{} forms])]
    (doseq [t types
            :let [replaces (form-replaces extra-parts t)]]
       (when (type-table-magic t)
         (swap! deftypes-magic update-in [t] concat
              (handle-forms t replaces forms))))
    :ok))

(defmacro spit-code
  "Emits specialized versions of collected forms"
  []
  `(do  ;let [start# (System/currentTimeMillis)]
        ;(println (str "declares: " (- start# (System/currentTimeMillis))))
     ~@(map #(list 'declare %) (keys @defns-magic))
     ~@(vals @deftypes-magic)
     ~@(vals @defns-magic)
     ~@(map (fn [t] `(imp/register-implementation
                      (~(add-fn-suffix t 'empty-ndarray) [1])))
            (keys type-table-magic))))

(defmacro specialize
  "Allows use of the 'magic' machinery from outside of `ndarray` namespace. This
   is useful for `loop-over`. An example can be found in
   `test-ndarray-implementation` namespace"
  [type & body]
  (let [replaces (form-replaces {} type)
        replaces (assoc replaces
                   'typename#
                   (symbol (str "clojure.core.matrix.impl.ndarray."
                                (get replaces 'typename#))))]
    `(do ~@(handle-forms type replaces body))))
