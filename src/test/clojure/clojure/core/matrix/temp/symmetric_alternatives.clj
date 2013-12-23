;; Experiments to compare speeds of implementations of symmetric?
(ns tst
  (:require [criterium.core :as c]
            [clojure.core.matrix :as mx]
            [clojure.core.matrix.protocols :as mp]))

(set! *warn-on-reflection* true)

;; This is much slower--3X to 4X than the other versions that don't create a seq of indexes.
(defn comp-sym?
  "Returns true if matrix is symmetric, false otherwise."
  [m]
  (and (mx/square? m)
       (every? (fn [[i j]] 
                 (= (mx/mget m i j) (mx/mget m j i)))
               (let [dim (first (mx/shape m))]
                 (for [i (range dim)
                       j (range dim)
                       :when (> j i)] ; no need to test both i,j and j,i since we do both at once. always true for (= j i).
                   [i j])))))

(defn mget-letfn-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))] ; 1 past the last valid index
         (letfn [(f [i j]
                   (cond 
                     (>= i dim) true  ; got all the way through--it's symmetric
                     (>= j dim) (recur (+ 1 i) (+ 2 i)) ; got through j's--start again with new i
                     (= (mx/mget m i j) 
                        (mx/mget m j i)) (recur i (inc j)) ; same, so check next pair
                     :else false))] ; not same, not symmetric. we're done.  
           (f 0 1)))))

;; This is 20-40% slower than mget-letfn-sym.  Why?
(defn mget-loop-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))]
         (loop [i 0 
                j 1]
           (cond 
             (>= i dim) true                    ; checked all i's, j's
             (>= j dim) (recur (+ 1 i) (+ 2 i)) ; checked j's: restart with new i
             (= (mx/mget m i j) 
                (mx/mget m j i)) (recur i (inc j)) ; i,j = j,i: try next j
             :else false))))) ; not equal: matrix isn't symmetric

(defn get-2d-letfn-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))] ; 1 past the last valid index
         (letfn [(f [i j]
                   (cond 
                     (>= i dim) true  ; got all the way through--it's symmetric
                     (>= j dim) (recur (+ 1 i) (+ 2 i)) ; got through j's--start again with new i
                     (= (mp/get-2d m i j) 
                        (mp/get-2d m j i)) (recur i (inc j)) ; same, so check next pair
                     :else false))] ; not same, not symmetric. we're done.  
           (f 0 1)))))

;; This is 20-40% slower than get-2d-letfn-sym.  Why?
(defn get-2d-loop-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))]
         (loop [i 0 
                j 1]
           (cond 
             (>= i dim) true                    ; checked all i's, j's
             (>= j dim) (recur (+ 1 i) (+ 2 i)) ; checked j's: restart with new i
             (= (mp/get-2d m i j) 
                (mp/get-2d m j i)) (recur i (inc j)) ; i,j = j,i: try next j
             :else false))))) ; not equal: matrix isn't symmetric

(defn hinted-get-2d-letfn-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))] ; 1 past the last valid index
         (letfn [(f [^long i ^long j]
                   (cond 
                     (>= i dim) true  ; got all the way through--it's symmetric
                     (>= j dim) (recur (+ 1 i) (+ 2 i)) ; got through j's--start again with new i
                     (= (mp/get-2d m i j) 
                        (mp/get-2d m j i)) (recur i (inc j)) ; same, so check next pair
                     :else false))] ; not same, not symmetric. we're done.  
           (f 0 1)))))

(defn hinted-get-2d-loop-sym?
  [m]
  (and (mx/square? m)
       (let [dim (first (mx/shape m))]
         (loop [i (long 0)
                j (long 1)]
           (cond 
             (>= i dim) true                    ; checked all i's, j's
             (>= j dim) (recur (+ 1 i) (+ 2 i)) ; checked j's: restart with new i
             (= (mp/get-2d m i j) 
                (mp/get-2d m j i)) (recur i (inc j)) ; i,j = j,i: try next j
             :else false))))) ; not equal: matrix isn't symmetric

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test alternatives with matrix provided by user
(defn bench-mat 
  [m]
  (let [mv (mx/matrix :vectorz m)
        mn (mx/matrix :ndarray m)
        mp (mx/matrix :persistent-vector m)
        mc (mx/matrix :clatrix m)
       ]

    (println "\n*** Using supplied matrix ***")

    ;(println "\n============= vectorz =============")

    ;(println "\nvectorz comp-sym?:")
    ;(c/bench (def _ (comp-sym? mv)))
    ;(println "\nvectorz mget-letfn-sym?:")
    ;(c/bench (def _ (mget-letfn-sym? mv)))
    ;(println "\nvectorz mget-loop-sym?:")
    ;(c/bench (def _ (mget-loop-sym? mv)))
    ;(println "\nvectorz symmetric?:")
    ;(c/bench (def _ (mx/symmetric? mv)))

    (println "\n============= ndarray =============")

    ;(println "\nndarray comp-sym?:")
    ;(c/bench (def _ (comp-sym? mn)))
    (println "\nndarray mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? mn)))
    (println "\nndarray mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? mn)))
    (println "\nndarray get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? mn)))
    (println "\nndarray get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? mn)))
    (println "\nndarray symmetric?:")
    (c/bench (def _ (mx/symmetric? mn)))
    (println "\nndarray mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? mn)))

    (println "\n============= persistent-vector =============")

    ;(println "\npersistent-vector comp-sym?:")
    ;(c/bench (def _ (comp-sym? mp)))
    (println "\npersistent-vector mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? mp)))
    (println "\npersistent-vector mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? mp)))
    (println "\npersistent-vector get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? mp)))
    (println "\npersistent-vector get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? mp)))
    (println "\npersistent-vector symmetric?:")
    (c/bench (def _ (mx/symmetric? mp)))
    (println "\npersistent-vector mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? mp)))

    (println "\n============= clatrix =============")

    ;(println "\nclatrix comp-sym?:")
    ;(c/bench (def _ (comp-sym? mc)))
    (println "\nclatrix mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? mc)))
    (println "\nclatrix mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? mc)))
    (println "\nclatrix get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? mc)))
    (println "\nclatrix get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? mc)))
    (println "\nclatrix symmetric?:")
    (c/bench (def _ (mx/symmetric? mc)))
    (println "\nclatrix mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? mc)))
  ))


;; test alternatives with a zero matrix of size dimXdim
(defn bench-zero
  [dim]
  (println (str "\n*** Using zero matrices of size " dim "x" dim " ***"))

  ;(println "\n============= vectorz =============")

;  (let [m (mx/new-matrix :vectorz dim dim)]
;    ;(println "\nvectorz comp-sym?:")
;    ;(c/bench (def _ (comp-sym? m)))
;    (println "\nvectorz mget-letfn-sym?:")
;    (c/bench (def _ (mget-letfn-sym? m)))
;    (println "\nvectorz mget-loop-sym?:")
;    (c/bench (def _ (mget-loop-sym? m)))
;    (println "\nvectorz symmetric?:")
;    (c/bench (def _ (mx/symmetric? m)))
;    )

  (println "\n============= ndarray =============")

  (let [m (mx/new-matrix :ndarray dim dim)]
    ;(println "\nndarray comp-sym?:")
    ;(c/bench (def _ (comp-sym? m)))
    (println "\nndarray mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? m)))
    (println "\nndarray mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? m)))
    (println "\nndarray get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? m)))
    (println "\nndarray get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? m)))
    (println "\nndarray symmetric?:")
    (c/bench (def _ (mx/symmetric? m)))
    (println "\nndarray mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? m)))
    )

  (println "\n============= persistent-vector =============")

  (let [m (mx/new-matrix :persistent-vector dim dim)]
    ;(println "\npersistent-vector comp-sym?:")
    ;(c/bench (def _ (comp-sym? m)))
    (println "\npersistent-vector mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? m)))
    (println "\npersistent-vector mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? m)))
    (println "\npersistent-vector get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? m)))
    (println "\npersistent-vector get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? m)))
    (println "\npersistent-vector symmetric?:")
    (c/bench (def _ (mx/symmetric? m)))
    (println "\npersistent-vector mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? m)))
    )

  (println "\n============= clatrix =============")

  (let [m (mx/new-matrix :clatrix dim dim)]
    ;(println "\nclatrix comp-sym?:")
    ;(c/bench (def _ (comp-sym? m)))
    (println "\nclatrix mget-letfn-sym?:")
    (c/bench (def _ (mget-letfn-sym? m)))
    (println "\nclatrix mget-loop-sym?:")
    (c/bench (def _ (mget-loop-sym? m)))
    (println "\nclatrix get-2d-letfn-sym?:")
    (c/bench (def _ (get-2d-letfn-sym? m)))
    (println "\nclatrix get-2d-loop-sym?:")
    (c/bench (def _ (get-2d-loop-sym? m)))
    (println "\nclatrix symmetric?:")
    (c/bench (def _ (mx/symmetric? m)))
    (println "\nclatrix mget-loop-symmetric?:")
    (c/bench (def _ (mx/loop-symmetric? m)))
  )
)
