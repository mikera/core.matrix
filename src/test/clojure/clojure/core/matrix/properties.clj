(ns clojure.core.matrix.properties
  (:use clojure.core.matrix)
  (:use clojure.test)
  (:require [simple-check.core       :as sc]
            [simple-check.generators :as gen]
            [simple-check.properties :as prop]
            [simple-check.clojure-test :as ct :refer (defspec)]))

;; # Global constants
;;
;; This constant defines a number of tests for each of properties

(def num-tests 100)

;; # Helpers
;;
;; ## Generators

;; TODO: we can't use vectorz or doubles here until simple-check is updated,
;; because now 2.0 != 2
(def gen-impl
  (gen/elements [:ndarray :persistent-vector]))

;; TODO: remove this as soon as simple-check is updated
(defn gen-vector
  "Create a generator whose elements are chosen from `gen`. The count of the
  vector will be bounded by the `size` generator parameter."
  [gen num-elements]
  [:gen (fn [rand-seed size]
          (vec (repeatedly num-elements #(gen/call-gen gen rand-seed size))))])

;; TODO: n should be generated as well
(defn gen-vec-mtx [n]
  (gen-vector (gen-vector gen/int n) n))

;; TODO: write N-Dimensional array generator

;; TODO: submit this to simple-check
(def gen-strictly-pos-int
  [:gen (fn [rand-seed size]
          (gen/call-gen (gen/choose 1 (inc size))
                        rand-seed size))])

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
      (= vec-mtx (to-nested-vectors mtx)))))

;; Check if we can construct an array and get nested vectors back
;; TODO: here we should use N-Dimensional array
(defspec array-constructible num-tests
  (prop/for-all [impl gen-impl
                 vec-mtx (gen-vec-mtx 5)]
    (let [mtx (array impl vec-mtx)]
      (= vec-mtx (to-nested-vectors mtx)))))

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
                 rows gen-strictly-pos-int
                 cols gen-strictly-pos-int]
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
