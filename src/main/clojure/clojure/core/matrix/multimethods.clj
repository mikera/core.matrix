(ns clojure.core.matrix.multimethods)

;; multimethods are provided for when an implementation does not have a specific fast path
;; for performing a matrix operation. Enables several options:
;; - other implementations to provide a better handler
;; - a default implementation may be used

(defmulti mul
  (fn [x y] [(class x) (class y)]))

;; heirarchies for matrix types

;; square matrices
(derive ::square-matrix ::matrix)
(derive ::diagonal-matrix ::square-matrix)
(derive ::scalar-matrix ::diagonal-matrix)
(derive ::identity-matrix ::scalar-matrix)
(derive ::lower-triangular-matrix ::square-matrix)
(derive ::upper-triangular-matrix ::square-matrix)

;; sparse matrices
(derive ::sparse-matrix ::matrix)

;; specialised matrix sizes
(derive ::matrix-3x3 ::square-matrix)
(derive ::matrix-2x2 ::square-matrix)

;; specialised 3d matrices
(derive ::matrix-rotation-3d ::matrix-3x3)




