(ns clojure.core.matrix.demo.geom
  "Demonstration of geometric functionality using core.matrix"
  (:require [clojure.core.matrix :refer :all]))

;; =========================================================================
;; Vector rotation example

;; The Pi constant.
(def π 3.141592653589793)

;; The Tau constant. Better than Pi.
;; http://tauday.com/tau-manifesto
(def τ (* 2.0 π))

(defn rot 
  "Rotates a 2D vector by a given angle (in turns, i.e. 180 degrees = 0.5)"
  ([turns]
    (let [a (* τ turns)]
      [[  (cos a)  (sin a)]
       [(-(sin a)) (cos a)]])))

;; Rotate a vector by a 1/8 turn
(mmul (rot 1/8) [3 4])
;; [4.949747468305833 0.7071067811865479]


