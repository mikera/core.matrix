(ns clojure.core.matrix.properties
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

;; Property based testing of randomly generated core.matrix arrays

;; # Global constants
;;
;; This constant defines a number of tests for each of properties

(def num-tests 100)

;; # Helpers
;;
;; ## Generators

(def gen-impl
  (gen/elements [:ndarray :persistent-vector :vectorz :object-array :double-array]))

;; TODO: n should be generated as well
(defn gen-vec-mtx [n]
  (gen/vector (gen/vector gen/int n) n))

;; TODO: write N-Dimensional array generator

;; ## Predicates

(defn proper-matrix?
  "Check if provided nested vectors form a proper matrix — that is, all nested
   vectors have the same length"
  [mtx]
  (let [first-size (count (first mtx))]
    (every? (partial = first-size) (map count mtx))))

;; TODO: write proper-array?

;; # General functionality tests
;;
;; Check if we can construct matrix and get nested vectors back

(defspec matrix-constructible num-tests
  (prop/for-all [impl gen-impl
                 vec-mtx (gen-vec-mtx 5)]
    (let [mtx (matrix impl vec-mtx)]
      (equals vec-mtx (to-nested-vectors mtx)))))

;; Check if we can construct an array and get nested vectors back
;; TODO: here we should use N-Dimensional array
(defspec array-constructible num-tests
  (prop/for-all [impl gen-impl
                 vec-mtx (gen-vec-mtx 5)]
    (let [mtx (array impl vec-mtx)]
      (equals vec-mtx (to-nested-vectors mtx)))))

;; Check if new-vector returns zero- or null- filled vector of given size
(defspec new-vector-zero-filled num-tests
  (prop/for-all [impl gen-impl
                 l gen/pos-int]
    (let [vec (new-vector l)
          vec-vec (to-nested-vectors vec)]
      (and (every? #(or (nil? %) (== 0 %))
                   vec-vec)
           (== l (count vec-vec))))))

;; Check if new-matrix returns zero- or null- filled matrix of given size
(defspec new-matrix-zero-filled num-tests
  (prop/for-all [impl gen-impl
                 rows gen/s-pos-int
                 cols gen/s-pos-int]
    (let [mtx (new-matrix rows cols)
          vec-mtx (to-nested-vectors mtx)]
      (and (every? #(or (nil? %) (== 0 %))
                   (flatten vec-mtx))
           (proper-matrix? vec-mtx)
           (== rows (count vec-mtx))
           (== cols (count (first vec-mtx)))))))

;; Check if new-array returns zero- or null- filled matrix of given size
;; TODO: find out why this hangs
#_(defspec new-array-zero-filled num-tests
  (prop/for-all [impl gen-impl
                 arr-shape (gen/vector gen-strictly-pos-int)]
    (let [arr (new-array arr-shape)
          vec-arr (to-nested-vectors arr)]
      (and (every? #(or (nil? %) (== 0 %))
                   (flatten vec-arr))
           #_(proper-array? vec-mtx)
           (= arr-shape (shape vec-arr))
           (= arr-shape (shape arr))))))

(defspec householder-matrix-props num-tests
  (prop/for-all [;; shrinking of keywords is broken in current simple-check
                 ;; impl (gen/elements [:vectorz])
                 v (gen/vector gen/int 5)]
    (let [v (->> v (array :vectorz) normalise)
          i (array :vectorz (identity-matrix 5))
          m (sub i (emul 2.0 (outer-product v v)))]
      (is (equals m (transpose m) 1.0E-12))
      (is (equals m (inverse m) 1.0E-12))
      (is (equals (mmul m m) i 1.0E-12)))))