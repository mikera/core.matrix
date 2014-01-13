(ns clojure.core.matrix.demo.geom
  (:use clojure.core.matrix))

(def π 3.141592653589793)

(def τ (* 2.0 π))

(defn rot [turns]
  (let [a (* τ turns)]
    [[  (cos a)  (sin a)]
     [(-(sin a)) (cos a)]]))

(mmul (rot 1/8) [3 4])


