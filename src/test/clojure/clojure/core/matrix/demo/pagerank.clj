(ns clojure.core.matrix.demo.pagerank
  (:use clojure.core.matrix))

;; =================================================================================
;; A demo of a simple Pagerank calculation
;;
;; For more info on background and algoritrhm see:
;;   - http://en.wikipedia.org/wiki/PageRank

;; link matrix: each row represnts the number outbound links from a page to other pages
(def link-matrix
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

(def DAMPING 0.85)

(defn norm-1 
  "Normalises a vector to a sum of 1.0."
  ([v]
    (let [s (esum v)]
      (if (== s 0.0)
        (let [n (ecount v)] 
          (array (repeat n (/ 1.0 n))))
        (scale v (/ 1.0 s))))))

;; =================================================================================
;; Iterative method
;;
;; each iteration of the pagerank sequences gets closer to the correct pagerank value

(defn pagerank 
  "Returns an infinite sequence of vectors that converges on the pagerank values,
   using an iterative method."
  ([links]
    (let [transitions (transpose (map norm-1 (slices links)))
          initial-rank (norm-1 (zero-vector (row-count links)))]
      (iterate 
        (fn [v] (add (mmul DAMPING transitions v)
                     (mmul (- 1.0 DAMPING) initial-rank)))
        initial-rank))))

(nth (pagerank link-matrix) 0)
;; => [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1]

(nth (pagerank link-matrix) 10)
;; => [0.10267022104015164 0.13709020713321848 0.05783843684746401 0.2835544691864015 0.015000000000000003 0.04909679698625842 0.15295646321406195 0.1398971838945996 0.04689622169784466 0.015000000000000003]

(nth (pagerank link-matrix) 100)
;; => [0.10268308453594212 0.13709906546448453 0.05783185760307677 0.2835419187399636 0.015000000000000003 0.049098055965063864 0.15296143983559468 0.13989401901156823 0.04689055884430651 0.015000000000000003]



;; =================================================================================
;; Direct (algebraic) method

(defn pagerank-direct 
  "Computes the pagerank directly"
  ([links]
    (let [transitions (transpose (map norm-1 (slices links)))
          n (row-count links)]
      (mmul (inverse (sub (identity-matrix n) (mul DAMPING transitions)))
            (vec (repeat n (/ (- 1.0 DAMPING) n)))))))

(pagerank-direct link-matrix)
;; => [0.1026830845359421 0.1370990654644845 0.05783185760307677 0.28354191873996354 0.015000000000000003 0.04909805596506385 0.15296143983559463 0.13989401901156823 0.04689055884430651 0.015000000000000003]
