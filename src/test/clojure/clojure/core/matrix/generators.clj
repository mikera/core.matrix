(ns clojure.core.matrix.generators
  (:require [clojure.test.check.generators :as gen]
            [clojure.core.matrix :refer :all]))

;; ===========================================
;; Implementation generators

(def gen-impl
  (gen/elements [:ndarray :persistent-vector :vectorz :object-array :double-array]))

;; ===========================================
;; Dimension, shape and scaling generators

(defn gen-scale 
  "Creates a generator that pre-modifies the 'size' pramater with the function f. Use if you want to 
   have the size grow at a different rate from the normal linear scaling."
  ([f gen]
    (let [gf (or (:gen gen) "gen paramter must be a test.check generator")
          new-gf (fn [rnd size]
                   (gf rnd (f size)))]
      (clojure.test.check.generators.Generator. new-gf))))


(def gen-dims
  "A generator for dimensionalities (including zero). Grows quite slowly."
  (gen-scale #(Math/pow (double %) 0.333) gen/pos-int))

(def gen-s-pos-dims
  "A generator for dimensionalities greated than or equal to one. Grows quite slowly."
  (gen/such-that pos? gen-dims 100))

(defn gen-shape 
  "Creates a generator that returns valid core.matrix shapes for arrays, with strictly positive 
   dimension sizes. Grows roughly linearly in the number of elements."
  ([]
    (gen-shape gen-dims))
  ([gen-dims]
    (gen/bind gen-dims
              (fn [dims]
                (gen/vector (gen-scale #(Math/pow (double %) (/ 1.0 (double dims))) gen/s-pos-int) 
                            dims)))))

(def gen-matrix-shape
  (gen-shape (gen/return 2)))

(def gen-vector-shape
  (gen/fmap vector gen/s-pos-int))

;; ===========================================
;; Array generators

(defn gen-array
  "Creates a generator that returns arrays"
  ([g-shape g-element]
    (gen-array g-shape g-element (current-implementation)))
  ([g-shape g-element impl-or-g-impl]
    (let [g-impl (if (gen/generator? impl-or-g-impl) impl-or-g-impl (gen/return impl-or-g-impl))]
      (gen/bind 
        g-shape
        (fn [shape]
          (gen/bind 
            g-impl
            (fn [impl]
              (gen/fmap
                (fn [elts]
                  (array impl (reshape elts shape)))
                (gen/vector g-element (reduce * 1 shape))))))))))

(defn gen-matrix
  "Creates a generator for 2 dimensional matrices. Uses the supplied generators for implementation 
   and elements if specified."
  ([g-element]
    (gen-array gen-matrix-shape g-element))
  ([g-element impl-or-g-impl]
    (gen-array gen-matrix-shape g-element impl-or-g-impl)))

(defn gen-vector
  "Creates a generator for 1 dimensional vectors. Uses the supplied generators for implementation 
   and elements if specified."
  ([g-element]
    (gen-array gen-vector-shape g-element))
  ([g-element impl-or-g-impl]
    (gen-array gen-vector-shape g-element impl-or-g-impl)))


;; =============================================================================
;; Scalar generators


(def gen-double (gen/fmap double gen/ratio))


(defn gen-nested-vectors
  "Generator for nested vectors of a given shape, using a specified element generator"
  ([shape elem-gen]
    (reduce (fn [g s] (gen/vector g s)) elem-gen (reverse shape))))
