(ns clojure.core.matrix.demo.snakes
  "Namespace for similating the probabilities of landing on each square in a snakes and ladders game."
  (:require [clojure.core.matrix :refer :all]))

;; The starting probability vector has:
;; - Probability 1.0 of being in the "start" position (position 0)
;; - Probability 0.0 of being on any other sqquare
(def start (vec (cons 1.0 (repeat 100 0.0))))

;; Position of snakes, as a map of (position of snake head) -> (position of snake tail)
(def snakes {16 6,
             49 11,
             47 26,
             56 53,
             62 19,
             64 60,
             87 24,
             93 73,
             95 75,
             98 78})

;; Position of snakes, as a map of (bottom of ladder) -> (top of ladder)
(def ladders {1 38,
              4 14,
              9 31,
              21 42,
              36 44,
              28 84,
              51 67,
              71 91,
              80 100})

;; The combined map of snakes and ladders, a map of (start position) -> (target position))
(def snakes-and-ladders (merge snakes ladders))

;; The function that updates the probability vector for each game step
(defn game-step 
  "Takes a vector pos of probabilities of being on each square. Returns a new vector representing the 
   probabilities of being on each square after one more dice roll"
  ([pos]
     (add ;; Add a vector representing the probability of where we may end after this roll 
          (apply add
                (for [start (range 0 100) ;; do for every position (excluding 100, the winning square)
                      roll (range 1 7)]   ;; do for every dice roll 1-6
                  (let [;; what is the probability we are at this stating position?
                        prob (pos start)  
                        
                        ;; what is the chance of being at this position *and* making the roll?
                        chance (/ prob 6)
                        
                        ;; target position 
                        target (+ start roll)
                        
                        ;; adjust target to "bounce" off end if winning position is overshot
                        target (if (<= target 100) target (- 100 (- target 100)))
                        
                        ;; adjust target if we hit a snake or ladder
                        target (if (snakes-and-ladders target) (snakes-and-ladders target) target)]
                    
                    ;; use core.matrix "join" operation to create the result vector
                    ;; i.e. with the chance of being at the target position
                    (join (repeat target 0.0) [chance] (repeat (- 100 target) 0.0)))))
          
          ;; a vector representing the chance that we have already won
          (join (repeat 100 0.0) [(pos 100)]))))

;; A lazy sequence of all the probability vectors after each turn
;; This is computed by simply iterating the game-step function over the start vector 
(def states (iterate game-step start))

;; A lazy vector of all the probabilities of being in the winning position 100
(def winning-chance (map last states))

(comment
  ;; run this at the REPL to see the cumulative winning probabilities after each move
  (take 50 winning-chance)
  
  ;; run this at the REPL to see the probability of winning on a specific move
  ;; i.e. the difference in cumulative winning chance
  (take 50 (map - winning-chance (cons 0.0 winning-chance)))
  
  )
