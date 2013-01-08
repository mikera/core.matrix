(ns core.matrix.multimethods)

(defmulti mul
  (fn [x y] [(.getClass x) (.getClass y)]))
