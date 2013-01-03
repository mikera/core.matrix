(ns clatrix.api)

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
  "Protocol to support matrix multiplication on an arbitrary matrix or vector"
  (mmultiply [m a]))

;; =============================================================
;; Functions operating on standard protocols
;;
;; API users should prefer these functions to using the protocols directly

(defn mget 
  "Gets a value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m x]
    (get-1d m x))
  ([m x y]
    (get-2d m x y))
  ([m x y & more]
    (get-nd m (cons x (cons y more)))))

;; ============================================================
;; Implementation for standard Clojure vectors used as matrices

(extend-protocol PIndexedAccess
  clojure.lang.IPersistentVector
    (get-1d [m x]
      (double (.nth m (int x))))
    (get-2d [m x y]
      (let [row (.nth m (int x))]
        (get-1d row y)))
    (get-nd [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.nth m (int (first indexes)))]
          (get-nd m next-indexes))
        (double (.nth m (int (first indexes)))))))

(defn coerce-nested 
  "Ensures a vector is fully coerced"
  ([v]
    (mapv #(if (number? %) % (coerce-nested v)) v)))

(extend-protocol PCoercion
  clojure.lang.IPersistentVector
    (coerce [m param]
      (cond
        (vector? param) param
        (sequential? param) (coerce-nested param)
        :default (error "Can't coerce to vector: " (class param)))))

(extend-protocol PMatrixMultiply
  clojure.lang.IPersistentVector
    (mmultiply [m a]
      (let [a (coerce-param m a)]
        (error "Not yet implemented"))))

;; ============================================================
;; Fallback implementations for stuff we don't recognise