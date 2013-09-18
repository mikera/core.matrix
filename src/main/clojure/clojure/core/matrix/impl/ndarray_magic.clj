(ns clojure.core.matrix.impl.ndarray-magic
  (:use clojure.tools.macro)
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:use clojure.core.matrix.impl.ndarray-magic)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)

(defn init [types]
  (def type-table-magic types)
  (def deftypes-magic (atom {}))
  (def defns-magic (atom {})))

(defn form-replaces [extra-parts t]
  (->> (merge (type-table-magic t) (extra-parts t))
       (map (juxt #(-> % key name (str "#") symbol) val))
       (into {})))

;; TODO: document symbol replacement and #t fn-suffix addition

(defn add-fn-suffix [t s]
  (if-let [suffix (->> t type-table-magic :fn-suffix)]
    (->> suffix name (str s "-") symbol)
    (symbol s)))

(defn handle-symbol [t replaces s]
  (let [new-s (or (replaces s)
                  (if (.endsWith (name s) "#t")
                    (->> s name (drop-last 2) (apply str) (add-fn-suffix t))
                    s))]
    (if-let [m (meta s)]
      (with-meta new-s
        (w/postwalk-replace replaces m))
      new-s)))

;; TODO: fix this dirty hack (!)
(defn handle-forms [t replaces form]
  (w/prewalk
   (fn [x]
     (if (symbol? x) (handle-symbol t replaces x)
         ;; this is dirty
         (if (and (seq? x) (symbol? (first x))
                  (#{"loop-over"
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
                     "fold-over-nd-internal"}
                   (name (first x))))
           (handle-forms t replaces (macroexpand-1 x))
           x)))
   form))

(defn handle-defn-form [t replaces [_ fn-name & _ :as form]]
  (let [new-fn-name (add-fn-suffix t fn-name)
        new-replaces (assoc replaces fn-name new-fn-name)]
    (handle-forms t new-replaces [new-fn-name form])
    #_(w/postwalk
     (fn [x]
       (if (symbol? x) (handle-symbol t new-replaces x)
           x))
     [new-fn-name form])))

(defmacro with-magic
  ([types form] `(with-magic ~types {} ~form))
  ([types extra-parts form]
     (doseq [t types
             :let [replaces (form-replaces extra-parts t)]]
       (when-not (type-table-magic t)
         (throw (IllegalArgumentException.
                 (str "there is no type " (name t) " in init-magic"))))
       (case (first form)
         deftype (swap! deftypes-magic assoc t
                        (handle-forms t replaces form))
         defn (swap! defns-magic conj
                     (handle-defn-form t replaces form))
         :else (throw (IllegalArgumentException.
                       "only deftype and defn are supported in with-magic"))))
     :ok))

(defmacro extend-types [types & forms]
  (let [[extra-parts forms] (if (map? (first forms))
                              [(first forms) (rest forms)]
                              [{} forms])]
    (doseq [t types
            :let [replaces (form-replaces extra-parts t)]]
       (when-not (type-table-magic t)
         (throw (IllegalArgumentException.
                 (str "there is no type " (name t) " in init-magic"))))
       (swap! deftypes-magic update-in [t] concat
              (handle-forms t replaces forms)))
    :ok))

;; TODO: it's possible to carry over line numbers manually through macro
;; expansion like this: `(-> &form meta :line)` in with-magic macro and
;; `(with-meta obj {:line (int line-num})` here
(defmacro spit-code []
  `(do
     ~@(map #(list 'declare %) (keys @defns-magic))
     ~@(vals @deftypes-magic)
     ~@(vals @defns-magic)
     ~@(map (fn [t] `(imp/register-implementation
                      (~(add-fn-suffix t 'empty-ndarray) [1])))
            (keys type-table-magic))))

(defmacro specialize [type & body]
  `(do ~@(handle-forms type (form-replaces {} type) body)))
