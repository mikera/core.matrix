(ns clatrix.api)

(defprotocol PIndexedAccess
  (get-1d [m x])
  (get-2d [m x y])
  (get-nd [m indexes]))

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