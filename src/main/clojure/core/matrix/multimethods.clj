(ns core.matrix.multimethods)

(defmulti mul
  (fn [x y] [(.getClass x) (.getClass y)]))

(derive ::square-matrix ::matrix)
(derive ::diagonal-matrix ::square-matrix)
(derive ::scalar-matrix ::diagonal-matrix)
(derive ::identity-matrix ::scalar-matrix)


