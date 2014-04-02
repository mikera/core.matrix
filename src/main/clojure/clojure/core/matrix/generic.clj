(ns clojure.core.matrix.generic
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.generic-protocols :as gmp])
  (:require [clojure.core.matrix.impl.generic-default])
  (:require [clojure.core.matrix.impl.default])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.implementations :as imp]))

;; Placeholder namespace for generic versions of core.matrix algorithms

;; generic element accessor functions
;; TODO: revisit these names?
(defn default-value
 "Returns the default value for the given array. Will normally be either nil or zero."
  ([impl]
     (mp/get-1d (mp/new-matrix-nd (imp/get-canonical-object impl) [1]) 0)))

(defrecord Specialisation
    [add mul div sub abs scalar? supports-inequality?
     zero one = < > >= <= sqrt])
;; generic versions of the clojure.core.matrix api

(defn mul
  "generic-version of clojure.core.matrix/mul"
  ([spec ] (:one spec))
  ([spec a] a)
  ([spec a b]
     (let [scalar? (:scalar? spec) mul (:mul spec)]
       (cond
        (scalar? b) (if (scalar? a) (mul a b) (gmp/generic-scale a b spec))
        (scalar? a) (gmp/generic-pre-scale b a spec)
        :else (gmp/generic-element-multiply a b spec))))
  ([spec a b & more]
     (reduce (partial mul spec) (mul spec a b) more)))

(defn emul
  "generic-version of clojure.core.matrix/emul"
  ([spec] (:one spec))
  ([spec a] a)
  ([spec a b]
    (gmp/generic-element-multiply a b spec))
  ([spec a b & more]
     (reduce #(gmp/generic-element-multiply %1 %2 spec)
             (gmp/generic-element-multiply a b spec) more)))

(defn mmul
  "generic-version of clojure.core.matrix/mmul"
  ([spec] (:one spec))
  ([spec a] a)
  ([spec a b]
    (gmp/generic-matrix-multiply a b spec))
  ([spec a b & more]
     (reduce #(gmp/generic-matrix-multiply %1 %2 spec)
             (gmp/generic-matrix-multiply a b spec) more)))

(defn e*
  "generic-version of clojure.core.matrix/e*"
  ([spec ] (:one spec))
  ([spec a] a)
  ([spec a b]
    (gmp/generic-element-multiply a b spec))
  ([spec a b & more]
     (reduce (partial e* spec) (e* spec a b) more)))

(defn div
  "generic-version of clojure.core.matrix/div"
  ([spec a] (gmp/generic-element-divide a spec))
  ([spec a b] (gmp/generic-element-divide a b spec))
  ([spec a b & more] (reduce #(gmp/generic-element-divide %1 %2 spec)
                             (gmp/generic-element-divide a b spec) more)))

(defn div!
  "generic-version of clojure.core.matrix/div!"
  ([spec a]
     (gmp/generic-element-divide! a spec)
     a)
  ([spec a b]
     (gmp/generic-element-divide! a b spec)
     a)
  ([spec a b & more]
     (gmp/generic-element-divide! a b spec)
     (doseq [c more]
       (gmp/generic-element-divide! a c spec))
     a))

(defn mul!
  "generic-version of clojure.core.matrix/mul!"
  ([spec a] a)
  ([spec a b]
    (gmp/generic-element-multiply! a b spec)
    a)
  ([spec a b & more]
    (gmp/generic-element-multiply! a b spec)
    (doseq [c more]
      (gmp/generic-element-multiply! a c spec))
    a))

(defn emul!
  "generic-version of clojure.core.matrix/emul!"
  ([spec a] a)
  ([spec a b]
    (gmp/generic-element-multiply! a b spec)
    a)
  ([spec a b & more]
    (gmp/generic-element-multiply! a b spec)
    (doseq [c more]
      (gmp/generic-element-multiply! a c spec))
    a))

(defn transform
  "generic-version of clojure.core.matrix/transform"
  [spec t v]
  (gmp/generic-vector-transform t v spec))

(defn transform!
  "generic-version of clojure.core.matrix/transform!"
  ([spec t v]
     (gmp/generic-vector-transform! t v spec)
     v))

(defn add
  "generic-version of clojure.core.matrix/add"
  ([spec a] a)
  ([spec a b]
    (gmp/generic-matrix-add a b spec))
  ([spec a b & more]
     (reduce #(gmp/generic-matrix-add %1 %2 spec)
             (gmp/generic-matrix-add a b spec) more)))

(defn add-product
  "generic-version of clojure.core.matrix/add-product"
  ([spec m a b]
    (gmp/generic-add-product m a b spec)))

(defn add-product!
  "generic-version of clojure.core.matrix/add-product!"
  ([spec m a b]
    (gmp/generic-add-product! m a b spec)
    m))

(defn add-scaled
  "generic-version of clojure.core.matrix/add-scaled"
  ([spec m a factor]
    (gmp/generic-add-scaled m a factor spec)))

(defn add-scaled!
  "generic-version of clojure.core.matrix/add-scaled!"
  ([spec m a factor]
    (gmp/generic-add-scaled! m a factor spec)
    m))

(defn add-scaled-product
  "generic-version of clojure.core.matrix/add-scaled-product"
  ([spec m a b factor]
    (gmp/generic-add-scaled-product m a b factor spec)))

(defn add-scaled-product!
  "generic-version of clojure.core.matrix/add-scaled-product!"
  ([spec m a b factor]
    (gmp/generic-add-scaled-product! m a b factor spec)
    m))

(defn sub
  "generic-version of clojure.core.matrix/sub"
  ([spec a] (gmp/generic-negate a spec))
  ([spec a b]
    (gmp/generic-matrix-sub a b spec))
  ([spec a b & more]
     (reduce #(gmp/generic-matrix-sub %1 %2 spec)
             (gmp/generic-matrix-sub a b spec) more)))

(defn add!
  "generic-version of clojure.core.matrix/add!"
  ([spec a] a)
  ([spec a b]
    (gmp/generic-matrix-add! a b spec)
    a)
  ([spec a b & more]
    (gmp/generic-matrix-add! a b spec)
    (doseq [m more] (gmp/generic-matrix-add! a m spec))
    a))

(defn sub!
  "generic-version of clojure.core.matrix/sub!"
  ([spec a] a)
  ([spec a b]
    (gmp/generic-matrix-sub! a b spec)
    a)
  ([spec a b & more]
    (gmp/generic-matrix-sub! a b spec)
    (doseq [m more] (gmp/generic-matrix-sub! a m spec))
    a))

(defn scale
  "generic-version of clojure.core.matrix/scale"
  ([spec m factor]
    (gmp/generic-scale m factor spec))
  ([spec m factor & more-factors]
     (gmp/generic-scale m (gmp/generic-element-multiply factor (reduce #(gmp/generic-element-multiply %1 %2 spec) more-factors) spec) spec)))

(defn scale!
  "generic-version of clojure.core.matrix/scale!"
  ([spec m factor]
    (gmp/generic-scale! m factor spec)
    m)
  ([spec m factor & more-factors]
     (gmp/generic-scale! m (gmp/generic-element-multiply factor (reduce #(gmp/generic-element-multiply %1 %2 spec) more-factors) spec) spec)
    m))

(defn square
  "generic-version of clojure.core.matrix/square"
  ([spec m]
    (gmp/generic-square m spec)))

(defn normalise
  "generic-version of clojure.core.matrix/normalise"
  ([spec v]
    (gmp/generic-normalise v spec)))

(defn normalise!
  "generic-version of clojure.core.matrix/normalise!"
  ([spec v]
    (gmp/generic-normalise! v spec)
    v))

(defn dot
  "generic-version of clojure.core.matrix/dot"
  ([spec a b]
    (or 
      (gmp/generic-vector-dot a b spec)
      (gmp/generic-inner-product a b spec))))

(defn inner-product
  "generic-version of clojure.core.matrix/inner-product"
  ([spec ] (:one spec))
  ([spec a]
    a)
  ([spec a b]
    (gmp/generic-inner-product a b spec))
  ([spec a b & more]
     (reduce #(gmp/generic-inner-product %1 %2 spec)
             (gmp/generic-inner-product a b spec) more)))

(defn outer-product
  "generic-version of clojure.core.matrix/outer-product"
  ([spec ] (:one spec))
  ([spec a] a)
  ([spec a b]
    (gmp/generic-outer-product a b spec))
  ([spec a b & more]
     (reduce (partial outer-product spec) (outer-product spec a b) more)))

(defn cross
  "generic-version of clojure.core.matrix/cross"
  ([spec a b]
     (gmp/generic-cross-product a b spec)))

(defn cross!
  "generic-version of clojure.core.matrix/cross!"
  ([spec a b]
    (gmp/generic-cross-product! a b spec)
    a))

(defn distance
  "generic-version of clojure.core.matrix/distance"
  ([spec a b]
    (gmp/generic-distance a b spec)))

;;TODO generic det and inverse

(defn negate
  "generic-version of clojure.core.matrix/negate"
  ([spec m]
    (gmp/generic-negate m spec)))

(defn negate!
  "generic-version of clojure.core.matrix/negate!"
  ([spec m]
     (gmp/generic-scale! m ((:sub spec) (:one spec)) spec)))

(defn trace

  "generic-version of clojure.core.matrix/trace"
  ([spec a]
     (gmp/generic-trace a spec)))

(defn length
  "generic-version of clojure.core.matrix/length"
  ([spec m]
    (gmp/generic-length m spec)))

(defn length-squared
  "generic-version of clojure.core.matrix/length-squared"
  ([spec m]
     (gmp/generic-length-squared m spec)))


(defn pow
  "generic-version of clojure.core.matrix/pow"
  ([spec m]
    m)
  ([spec m exponent]
    (gmp/generic-element-pow m exponent spec))
  ([spec m exponent & more]
     (reduce (fn [m x] (gmp/generic-element-pow m x spec))
             (gmp/generic-element-pow m exponent spec) more)))

(defn pow! 
  "generic-version of clojure.core.matrix/pow!"
  ([spec m a]
    ;; TODO: implement via a protocol + default implementation
     (mp/assign! m (pow spec m a))))

;;create all generic unary maths operators
(eval
  `(do ~@(map (fn [[name func]]
           `(defn ~name
              ~(str "generic version of clojure.core.matrix/" name) 
              ([~'spec ~'m]
                 (~(symbol "clojure.core.matrix.generic-protocols"
                           (str "generic-" name)) ~'m ~'spec)))) mops/maths-ops)
     ~@(map (fn [[name func]]
           `(defn ~(symbol (str name "!"))
              ~(str "generic version of clojure.core.matrix/" name) 
              ([~'spec ~'m]
                 (~(symbol "clojure.core.matrix.generic-protocols"
                           (str "generic-" name "!")) ~'m ~'spec)
                ~'m))) mops/maths-ops)))