(ns clojure.core.matrix.generators
  (:require [clojure.test.check.generators :as gen]
            [clojure.core.matrix :refer :all]))

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
  "Generator for arbitrary n-dimensional arrays"
  [& {:keys [max-elems min-elems
             max-dim min-dim
             implementations elem-gen]
      :or {max-elems 100 min-elems 1 max-dim 4 min-dim 0
           elem-gen gen-double
           implementations [:ndarray :persistent-vector :vectorz
                            :object-array :double-array]}}]
  (as-> (gen/elements (range min-dim (inc max-dim))) x
        (gen/bind x #(gen/vector gen/pos-int %))
        (gen/such-that #(<= min-elems (reduce * %) max-elems) x)
        (gen/bind x #(gen-nested-vectors % elem-gen))
        (gen/tuple (gen/elements implementations) x)
        (gen/fmap (fn [[impl data]] (array impl data)) x)))

(defn gen-matrix
  "generator for matrices"
  [& {:keys [implementations elem-gen shape-gen]
      :or {shape-gen (gen-shape :dimensionality 2)
           elem-gen gen-double
           implementations [:ndarray :persistent-vector :vectorz
                            :object-array :double-array]}}]
  (gen/bind shape-gen
            (fn [shape]
              (->> (gen-nested-vectors shape elem-gen)
                   (gen/tuple (gen/elements implementations))
                   (gen/fmap (fn [[impl data]] (matrix impl data)))))))
