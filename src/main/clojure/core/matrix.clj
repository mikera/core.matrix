(ns core.matrix
  (:refer-clojure :exclude [vector?]))

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
    `(throw (java.lang.RuntimeException. (str ~@vals)))))


;; =============================================================
;; API protocols
;;
;; Matrix implementations should extend all of these for full API support

;; protocol arity overloads behave oddly, so different names used for simplicity
(defprotocol PIndexedAccess
  "Protocol for indexed read access to matrices and vectors."
  (get-1d [m x])
  (get-2d [m x y])
  (get-nd [m indexes]))

(defprotocol PCoercion
  "Protocol to coerce a parameter to a format usable by a specific implementation. It is 
   up to the implementation to determine what parameter types they support" 
  (coerce-param [m param]))

(defprotocol PMatrixMultiply
  "Protocol to support matrix multiplication on an arbitrary matrix, vector or scalar"
  (matrix-multiply [m a])
  (scale [m a]))

(defprotocol PMatrixAdd
  "Protocol to support matrix addition on an arbitrary matrix or vector"
  (madd [m a]))

(defprotocol PMatrixDimensionInfo
  "Protocol to return standard dimension information about a matrix"
  (dimensionality [m])
  (vector? [m])
  (row-count [m])
  (column-count [m])
  (dimension-count [m x]))

(defprotocol PConversion
  "Protocol to allow conversion to Clojure-friendly vector format. Optional for implementers."
  (convert-to-nested-vectors [m]))

;; =============================================================
;; Functions operating on standard protocols
;;
;; API users should probably prefer these functions to using the protocols directly?

(defn matrix? 
  "Returns true if parameter is a valid matrix (any dimensionality)"
  ([m]
    (satisfies? PMatrixDimensionInfo m)))

(defn matrix-2d? 
  "Returns true if parameter is a regular matrix (2 dimensional matrix)"
  ([m]
    (and (matrix? m) (== 2 (dimensionality m)))))

(defn matrix-1d? 
  "Returns true if parameter is a 1 dimensional matrix"
  ([m]
    (and (matrix? m) (== 1 (dimensionality m)))))

(defn row-matrix?
  "Returns true if a matrix is a row-matrix"
  ([m]
    (== 1 (row-count m))))

(defn square?
  "Returns true if matrix is square (same number of rows and columns)"
  ([m]
    (== (row-count m) (column-count m))))

(defn column-matrix?
  "Returns true if a matrix is a column-matrix (same as vector?)"
  ([m]
    (== 1 (column-count m))))

(defn all-dimensions
  "Returns a sequence of the dimension counts for a matrix"
  ([m]
    (for [i (range (dimensionality m))] (dimension-count m i))))

(defn mget 
  "Gets a value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m x]
    (get-1d m x))
  ([m x y]
    (get-2d m x y))
  ([m x y & more]
    (get-nd m (cons x (cons y more)))))

(defn coerce
  "Coerces a parameter to a format usable by a specific matrix implementation"
  ([m a]
    (coerce-param m a)))

(defn mul
  "Performs matrix multiplication on matrices or vectors"
  ([a] a)
  ([a b]
    (if (number? b) 
      (scale a b)
      (matrix-multiply a b)))
  ([a b & more]
    (reduce mul (mul a b) more)))


;; ============================================================
;; Fallback implementations for stuff we don't recognise

(extend-protocol PConversion
  java.lang.Object
    (convert-to-nested-vectors [m]
      (error "Not yet implemented")))
