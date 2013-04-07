(ns clojure.core.matrix.impl.dataset)

;; TODO
;; a column based DataSet implementation.
;; columns are Java Object[] arrays??

(defrecord DataSet
  [column-names 
   column-types 
   columns]
  
  )