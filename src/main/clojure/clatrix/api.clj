(ns clatrix.api)

(defprotocol PIndexedAccess
  (get [x])
  (get [x y])
  (get-multi-dim [indexes]))

