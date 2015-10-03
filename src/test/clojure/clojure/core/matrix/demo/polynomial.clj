(ns clojure.core.matrix.demo.polynomial
  (:use clojure.core.matrix))

;; our task is to find a polynomial that fits a set of points

;; a sey of [x y points]
(def points [[0 1] [ 1 4] [2 10] [3 19] [4 31] [5 46]])

(def n (count points))

(def m 
  (matrix (mapv
           (fn [[x y]]
             (mapv (fn [i] (pow x i)) (range n)))
           points)))

(def y (mapv second points))

;; let x be the polynomial coefficients
;; we have m.x = y
;; so x= (inv m).y

(def x (mmul (inverse (array :vectorz m)) y))

;; now create a function that computes the polynomial
(defn f [t]
  (mmul x (mapv #(pow t %) (range n))))
