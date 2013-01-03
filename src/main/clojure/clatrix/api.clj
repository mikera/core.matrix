(ns clatrix.api)

(defprotocol PIndexedAccess
  (mget-1 [m x])
  (mget-2 [m x y])
  (mget-multi-dim [m indexes]))

(extend-protocol PIndexedAccess
  clojure.lang.IPersistentVector
    (mget-1 [m x]
      (double (.nth m (int x))))
    (mget-2 [m x y]
      (let [row (.nth m (int x))]
        (mget-1 row y)))
    (mget-multi-dim [m indexes]
      (if-let [next-indexes (next indexes)]
        (let [m (.nth m (int (first indexes)))]
          (mget-multi-dim m next-indexes))
        (double (.nth m (int (first indexes)))))))