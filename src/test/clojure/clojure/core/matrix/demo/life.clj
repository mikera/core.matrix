(ns clojure.core.matrix.demo.life
  "Demonstration of Game of Life implementation using core.matrix"
  (:require [clojure.core.matrix :refer :all]))

;; We represent the state of the world as a grid stored in a 10 x 10 matrix
;;   0 = dead cell
;;   1 = live cell

(def grid
  (array [[0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]
          [0 1 1 1 0 0 0 0 0 0 0]
          [0 0 0 1 0 0 0 0 0 0 0]
          [0 0 1 0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0 0 0 0]]))

(defn step [grid]
  (let [;; set of offsets to neighbouring cells
        offsets (for [i [-1 0 1] j [-1 0 1]] [i j])
        
        ;; rotate the grid using these offsets
        ;; note that you can use `shift` instead of `rotate` if you don't want a wraparound grid
        rotated-grids (map (partial rotate grid) offsets)
        
        ;; now sum up the rotated grids
        counts (apply add rotated-grids)]
    
    (eif ;; use element if to branch on the current state of each cell
         grid 
         
         ;; if live: stay alive if count is 3 or 4
         (add (eq counts 3) (eq counts 4)) 
         
         ;; if dead: come to life if count is exactly equal to 3
         (eq counts 3) )))

(comment
  ;; run this line to execute 10 steps of the game of life simulation and return each of the states
  (take 10 (iterate step grid))  
  
)
