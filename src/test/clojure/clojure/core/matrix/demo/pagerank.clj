(ns clojure.core.matrix.demo.pagerank
  (:refer-clojure :exclude [* - + / ==])
  (:require clojure.core.matrix.impl.ndarray)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators))

(defn demo []

;; =================================================================================
;; A demo of a simple Pagerank calculation
;;
;; For more info on background and algoritrhm see:
;;   - http://en.wikipedia.org/wiki/PageRank

(def n 10)

;; link matrix: each row represnts the number of outbound links from a page to other pages
(def links
  [[0 0 1 1 0 0 1 2 0 0]
   [1 0 0 1 0 0 0 0 0 0]
   [0 0 0 2 0 0 0 0 1 0]
   [0 1 0 0 0 0 1 0 0 0]
   [0 0 0 1 0 0 0 2 0 0]
   [2 0 0 0 0 0 0 0 1 0]
   [0 0 0 1 0 1 0 2 0 0]
   [0 0 1 4 0 0 0 0 0 0]
   [0 0 0 2 0 0 0 1 0 0]
   [1 1 1 1 0 1 0 2 1 0]])

(defn norm-1 
  "Normalises a vector to a sum of 1.0."
  ([v]
    (/ v (esum v))))

(norm-1 [1 2 3 4])

;; where do outbound visitors go, as a proportion?
(map norm-1 (rows links))
(pm *1)

;; transitions = inbound proportion
(def transitions (transpose (map norm-1 (rows links))))
(pm transitions)

;; chance of user clicking a link at each step
(def CLICK-THROUGH 0.85)

;; =================================================================================
;; Iterative method
;;
;; state defines the location of the browsing population
;; each iteration of the pagerank sequences gets closer to the correct pagerank value

(def initial-state (broadcast (/ 1.0 n) [n])) 
(pm initial-state)

(defn step [state]
  (add (* CLICK-THROUGH         (mmul transitions state))
       (* (- 1.0 CLICK-THROUGH) initial-state)))

(pm (step initial-state))

(def pageranks (iterate step initial-state))

(pm (nth pageranks 0))
;; => [0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100]

(pm (nth pageranks 1))
;; => [0.125 0.068 0.060 0.316 0.015 0.047 0.075 0.198 0.082 0.015]

(pm (nth pageranks 4))
;; => [0.108 0.138 0.057 0.280 0.015 0.050 0.153 0.140 0.046 0.015]

(pm (nth pageranks 10))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

(pm (nth pageranks 100))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

(pm (array (take 10 pageranks))) 

;; =================================================================================
;; Direct (algebraic) method

(defn pagerank-direct 
  "Computes the pagerank directly"
  ([transitions]
    (mmul (inverse (- (identity-matrix n) (* CLICK-THROUGH transitions)))
          (* (- 1.0 CLICK-THROUGH) initial-state))))

(pm (pagerank-direct transitions))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

)