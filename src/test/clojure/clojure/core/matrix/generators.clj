(ns clojure.core.matrix.generators
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

;; =============================================================================
;; Array generators for test.check generative testing
;;
;; TODO: should this be a separate library, to allow implementation to import it?

(def gen-double (gen/fmap double gen/ratio))


(defn gen-nested-vectors 
  "Generator for nested vectors in a given shape, using a specified element generator"
  ([shape elem-gen]
    (reduce (fn [g s] (gen/vector g s)) elem-gen (reverse shape))))

(defn gen-shape
  "Generator for valid core.matrix array shapes."
  ([] (gen/vector gen/s-pos-int))
  ([& {:keys [dimensionality]}] (gen/vector gen/s-pos-int dimensionality))) 

(defn gen-array
  "generator for n-dimensional arrays"
  [& {:keys [max-elems min-elems
             max-dim min-dim dimension-generator
             implementations elem-gen]
      :or {max-elems 100 min-elems 1 max-dim 4 min-dim 0
           elem-gen gen-double
           implementations [:ndarray :persistent-vector :vectorz
                            :object-array :double-array]}}]
  (let [dimension-generator (or dimension-generator
                                (gen/choose min-dim max-dim))]
    (as-> dimension-generator x
          (gen/such-that #(<= min-dim % max-dim) x)
          (gen/bind x #(gen/vector gen/pos-int %))
          (gen/such-that #(<= min-elems (reduce * %) max-elems) x)
          (gen/bind x #(gen-nested-vectors % elem-gen))
          (gen/tuple (gen/elements implementations) x)
          (gen/fmap (fn [[impl data]] (array impl data)) x))))

(defn gen-matrix
  "generator for n-dimensional matrices. Will pass options to gen-array"
  [& options]
  (apply gen-array :dimension-generator (gen/return 2) options))

(defn implicit-broadcastable? [l r]
  (let [sl (shape l) sr (shape r)]
    (and (>= (count sl) (count sr))
         (every? identity (map #(= %1 %2) (reverse sl) (reverse sr))))))

(defn gen-conforming-arrays
  "generates count arrays which have conforming shape"
  [count & options]
  (gen/such-that (fn [v]
                   (every? (fn [[l r]] (implicit-broadcastable? l r))
                           (partition 2 1 v)))
                 (gen/vector (apply gen-array options) count)))


(defn gen-conforming-matrices [count & options]
  (apply gen-conforming-arrays count :max-dim 2 :min-dim 2 options))
