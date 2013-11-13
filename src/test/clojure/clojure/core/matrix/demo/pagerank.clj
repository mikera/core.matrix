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
    (let [sum (esum v)]
      (div v sum))))

;; where do visitors go, as a pproportion?
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
;; each iteration of the pagerank sequences gets closer to the correct pagerank value

(def initial-rank (broadcast (/ 1.0 n) [n])) 

(defn step [rank]
  (add (mmul CLICK-THROUGH transitions rank)
       (mmul (- 1.0 CLICK-THROUGH) initial-rank)))

(step initial-rank)
(step *1)

(def pageranks (iterate step initial-rank))

(nth pageranks 0)
;; => [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1]

(nth pageranks 1)
;; => [0.12479166666666668 0.068125 0.05962500000000001 0.3160416666666667 0.015000000000000003 0.046875 0.07450000000000001 0.19775000000000004 0.08229166666666667 0.015000000000000003]

(nth pageranks 10)
;; => [0.10267022104015164 0.13709020713321848 0.05783843684746401 0.2835544691864015 0.015000000000000003 0.04909679698625842 0.15295646321406195 0.1398971838945996 0.04689622169784466 0.015000000000000003]

(nth pageranks 100)
;; => [0.10268308453594212 0.13709906546448453 0.05783185760307677 0.2835419187399636 0.015000000000000003 0.049098055965063864 0.15296143983559468 0.13989401901156823 0.04689055884430651 0.015000000000000003]

(defn pagerank 
  "Returns an infinite sequence of vectors that converges on the pagerank values,
   using an iterative method."
  ([links]
    (let [transitions (transpose (map norm-1 (slices links)))
          initial-rank (norm-1 (zero-vector (row-count links)))]
      (iterate 
        (fn [v] (add (mmul CLICK-THROUGH transitions v)
                     (mmul (- 1.0 CLICK-THROUGH) initial-rank)))
        initial-rank))))

;; =================================================================================
;; Direct (algebraic) method

(defn pagerank-direct 
  "Computes the pagerank directly"
  ([links]
    (mmul (inverse (sub (identity-matrix n) (mul CLICK-THROUGH transitions)))
            (vec (repeat n (/ (- 1.0 CLICK-THROUGH) n))))))

(pagerank-direct links)
;; => [0.1026830845359421 0.1370990654644845 0.05783185760307677 0.28354191873996354 0.015000000000000003 0.04909805596506385 0.15296143983559463 0.13989401901156823 0.04689055884430651 0.015000000000000003]

)