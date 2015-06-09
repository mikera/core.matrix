(ns clojure.core.matrix.demo.snakes
  (:require [clojure.core.matrix :refer :all]))

(def start (vec (cons 1.0 (repeat 100 0.0))))

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

(def ladders {1 38,
              4 14,
              9 31,
              21 42,
              36 44,
              28 84,
              51 67,
              71 91,
              80 100})

(def snakes-and-ladders (merge snakes ladders))

(defn accumulate [pos]
     (add (apply add
                (for [start (range 0 100)
                      roll (range 1 7)]
                  (let [chance (/ (pos start) 6)
                        target (+ start roll)
                        end (if (<= target 100) target (- 100 (- target 100)))
                        end (if (snakes-and-ladders end) (snakes-and-ladders end) end)]
                    (vec (concat (repeat end 0.0) [chance] (repeat (- 100 end) 0.0))))))
          (vec (concat (repeat 100 0.0) [(pos 100)]))))

(def states (iterate accumulate start))

(def winning-chance (map last states))
