(ns clojure.core.matrix.demo.normalise
  (:use [clojure.core.matrix]
        [clojure.core.matrix.stats]))

(def data [[1 2 10]
           [0 1 13]
           [0 2 11]
           [1 3 16]])

;; get the columns
(def cols (slices data 1))

;; compute means and standard deviations for each column
(def means (mapv mean cols))
(def sds (mapv sd cols))

;; vector of normalising functions, on for each column
(def normalisers (mapv (fn [mean sd] 
                         (fn [x] (/ (- x mean) sd))) 
                       means sds))

;; define a coder that normalises a row 
(def normalising-coder 
  (fn [row] (mapv #(% %2) normalisers (eseq row))))

(comment 
  (pm (map normalising-coder (rows data)))
)
;; [[ 0.866  0.000 -0.945]
;;  [-0.866 -1.225  0.189]
;;  [-0.866  0.000 -0.567]
;;  [ 0.866  1.225  1.323]]