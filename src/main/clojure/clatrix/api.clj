(ns clatrix.api)

(defprotocol PIndexedAccess
  (get-1d [m x])
  (get-2d [m x y])
  (get-nd [m indexes]))

(defn mget 
  "Gets a value from a matrix at a specified position. Supports any number of matrix dimensions."
  ([m x]
    (get-1d m x))
  ([m x y]
    (get-2d m x y))
  ([m x y & more]
    (get-nd m (cons x (cons y more)))))

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