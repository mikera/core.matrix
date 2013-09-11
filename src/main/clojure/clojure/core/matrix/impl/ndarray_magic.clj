(ns clojure.core.matrix.impl.ndarray-magic
  (:use clojure.tools.macro)
  (:require [clojure.walk :as w])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:refer-clojure :exclude [vector?]))

(set! *warn-on-reflection* true)

(defn init [types]
  (def type-table-magic types)
  (def deftypes-magic (atom {}))
  (def defns-magic (atom {})))

(defn magic-symbol [sym]
  (symbol (str "$" sym "$")))

(defn replace-meta [smap form]
  (if-let [m (meta form)]
    (let [new-meta (w/postwalk-replace smap m)]
      (with-meta form new-meta))
    form))

(defn replace-with-meta [smap form]
  (if-let [new-form (get smap form)]
    (if-let [m (meta form)]
      (replace-meta smap (with-meta new-form m))
      new-form)
    form))

(defn add-sym-suffix
  "Adds a suffix to symbol; returns an original symbol if suffix is nil"
  [sym suffix]
  (if suffix
    (symbol (str sym "-" suffix))
    sym))

(defn rename-suffixed
  "If provided form is a symbol of the form `$foo$t`, returns it as
   foo-SUFFIX, preserving metadata; if it's not, returns an original form"
  [form suffix]
  (if (symbol? form)
    (if-let [[_ sym-str] (re-find #"\$(.+)\.s\$$" (str form))]
      (with-meta (add-sym-suffix (symbol sym-str) suffix) (meta form))
      form)
    form))

(defn expand-deftypes [form]
  (if (seq? form)
    (case (first form)
      deftype form #_(macroexpand-1 form)
      form)
    form))

(defmacro specialize [type & body]
  (let [symbol-bindings (->> (for [[k v] (get type-table-magic type)]
                               [(magic-symbol (name k)) v])
                             (apply concat))
        symbol-map (apply hash-map symbol-bindings)
        suffix (-> type-table-magic type :fn-suffix)]
    ;; Here we need to process meta before AND after macro-expansion
    ;; because some macroses (like defn) take some of their arguments
    ;; and put them in metadata (defn stores arguments in :arglists
    ;; metadata), so we are dealing with metadata of metadata. To
    ;; avoid this, it's easier to fix metadata before macroexpansion
    ;; too; there are still cases when this hack will not work, though
    `(symbol-macrolet [~@symbol-bindings]
       ~@(->> body
              (w/postwalk #(rename-suffixed % suffix))
              (w/postwalk (partial replace-meta symbol-map))
              (w/postwalk (partial replace-with-meta symbol-map))
              #_(w/prewalk expand-deftypes)
              (mexpand-all)
              (w/postwalk #(rename-suffixed % suffix))
              (w/postwalk (partial replace-meta symbol-map))))))

(defmacro with-magic [types form]
  (doseq [type types]
    (iae-when-not (get type-table-magic type)
      (str "there is no type " (name type) " in init-magic"))
    (case (first form)
      deftype (do (swap! deftypes-magic conj
                         [type form]))
      defn (let [name (second form) ; (defn foobar <- second ...)
                 suffix (-> type-table-magic type :fn-suffix)
                 name-suffixed (rename-suffixed name suffix)]
             (swap! defns-magic conj
                    [[type name-suffixed] form]))
      (iae "only deftype and defn are supported in with-magic")))
  :ok)

(defmacro extend-types [types & forms]
  (doseq [type types]
    (iae-when-not (get type-table-magic type)
      (str "there is no type " (name type) " in init-magic"))
    (swap! deftypes-magic update-in [type] concat forms))
  :ok)

(defmacro spit-code []
  (let [declares (for [[_ name-suffixed] (keys @defns-magic)]
                   `(declare ~name-suffixed))
        deftypes (for [[type deftype-form] @deftypes-magic]
                   `(specialize ~type ~deftype-form))
        defns (for [[[type _] defn-form] @defns-magic]
                `(specialize ~type ~defn-form))
        regs (for [type (keys type-table-magic)]
               `(specialize ~type
                  (imp/register-implementation (~'$empty-ndarray.s$ [1]))))]
    `(do
       ~@declares
       ~@deftypes
       ~@defns
       ~@regs

       )))

;; (init-magic
;;  {:object {:regname :ndarray
;;            :fn-suffix nil
;;            :typename 'NDArray
;;            :array-tag 'objects
;;            :array-cast 'object-array
;;            :type-cast 'identity
;;            :type-object java.lang.Object}
;;   :long {:regname :ndarray-long
;;          :fn-suffix 'long
;;          :typename 'NDArrayLong
;;          :array-tag 'longs
;;          :array-cast 'long-array
;;          :type-cast 'long
;;          :type-object Long/TYPE}
;;   :float {:regname :ndarray-float
;;           :fn-suffix 'float
;;           :typename 'NDArrayFloat
;;           :array-tag 'floats
;;           :array-cast 'float-array
;;           :type-cast 'float
;;           :type-object Float/TYPE}
;;   :double {:regname :ndarray-double
;;            :fn-suffix 'double
;;            :typename 'NDArrayDouble
;;            :array-tag 'doubles
;;            :array-cast 'double-array
;;            :type-cast 'double
;;            :type-object Double/TYPE}})

;; (with-magic [:long :object] (defn $foo-suffixed$ [^$array-tag$ x] (aget x 0)))
;; (spit-code-magic)

;; (defmacro caster [x]
;;   `(~'$type$ ~x))

;; (defmacro looper [& body]
;;   `(macrolet [(~'continue [x#] `(prn "continue" ~x#))
;;               (~'break [x#] `(prn "break" ~x#))]
;;      ~@body))

;; (specialize :int
;;  (defn test-getter [x]
;;    (let [^$type$ x x]
;;      (prn "my type" $type$)
;;      (caster x)
;;      (looper (if (> (aget x 0) 1) (continue 3) (break 4)))
;;      (aget x 0)))

;;  (defn test-setter [x]
;;    (let [^$type$ x x]
;;      (prn "my type" $type$)
;;      (caster x)
;;      (aset x 0 13))))

;; Local Variables:
;; eval: (put-clojure-indent 'c-for 'defun)
;; eval: (put-clojure-indent 'iae-when-not 'defun)
;; eval: (put-clojure-indent 'macrolet 1)
;; eval: (put-clojure-indent 'symbol-macrolet 1)
;; End:
