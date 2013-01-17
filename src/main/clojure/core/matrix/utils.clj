(ns core.matrix.utils)

;; these are copies of methods from the library
;;   https://github.com/mikera/clojure-utils
;;
;; duplicated here to avoid an extra dependency

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))

;; useful TODO macro facilitates searching for TODO while throwing an error at runtime :-)
(defmacro TODO 
  ([]
    `(error "TODO: noy yet implemented"))) 
