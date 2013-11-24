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

(def links
;; link matrix: each row represnts the number of 
;; outbound links from a page to other pages
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

(def n (row-count links))

(defn proportions 
  "Normalises a vector to a sum of 1.0"
  ([v]
    (/ v (esum v))))

(proportions [1 2 3 4])

;; where do outbound visitors go, as a proportion?
(def outbound (array (map proportions (rows links))))
outbound
(pm outbound)

;; convert outbound to inbound proportions
(def inbound (transpose outbound))
(pm inbound)

;; chance of user clicking a link at each step
(def CLICK-THROUGH 0.85)

;; =================================================================================
;; Iterative method
;;
;; state defines the location of the browsing population
;; each iteration of the pagerank sequences gets 
;; closer to the correct pagerank value

(def initial-state (proportions (repeat n 1000000))) 
(pm initial-state)

(defn step 
  "Compute the next state, i.e. the proportion of people 
   on each page"
  ([state]
    (+ (* CLICK-THROUGH         (mmul inbound state))
       (* (- 1.0 CLICK-THROUGH) initial-state))))

(pm (step initial-state))

(def pageranks (iterate step initial-state))

(pm (nth pageranks 0))
;; => [0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100]

(pm (nth pageranks 4))
;; => [0.108 0.138 0.057 0.280 0.015 0.050 0.153 0.140 0.046 0.015]

(pm (nth pageranks 10))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

(pm (nth pageranks 100))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

;; has it converged? if so this should be near zero
(pm (- (nth pageranks 100) (nth pageranks 300)))

;; make array out of sequence of steps
(pm (array (take 8 pageranks))) 

;; =================================================================================
;; Direct (algebraic) method

(defn pagerank-direct 
  "Computes the pagerank directly"
  ([inbound]
    (mmul (inverse (- (identity-matrix n) 
                      (* CLICK-THROUGH inbound)))
          (* (- 1.0 CLICK-THROUGH) initial-state))))

(pm (pagerank-direct inbound))
;; => [0.103 0.137 0.058 0.284 0.015 0.049 0.153 0.140 0.047 0.015]

)