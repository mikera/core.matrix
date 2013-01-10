(ns core.matrix.utils)


(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))

(defmacro TODO 
  ([]
    `(error "TODO: noy yet implemented"))) 
