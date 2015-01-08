(ns clojure.core.matrix.demo.matrix-joining
  (:use [clojure.core.matrix]))

(defn join-all
   "Joins a matrix of matrices to create a bigger matrix :-)"
   ([M]
     (apply join (map #(apply join-along 1 (slices %)) (slices M)))))

;; EXAMPLE CODE

(join-all [[ [[1]]      [[2 3]]        ]
           [ [[4] [7]]  [[5 6] [8 9]]  ]])
; => [[1 2 3] [4 5 6] [7 8 9]]