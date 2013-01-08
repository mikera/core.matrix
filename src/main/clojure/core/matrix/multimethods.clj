(ns core.matrix.multimethods)

;; multimethods are provided for when an implementation does not have a specific fast path
;; for performing a matrix operation. Enables several options:
;; - other implementations to provide a better handler
;; - a default implementation may be used

(defmulti mul
  (fn [x y] [(.getClass x) (.getClass y)]))

;; heirarchies for matrix types

(derive ::square-matrix ::matrix)
(derive ::diagonal-matrix ::square-matrix)
(derive ::scalar-matrix ::diagonal-matrix)
(derive ::identity-matrix ::scalar-matrix)


