(ns clojure.core.matrix.docgen.bench-suite)

;; # Utils

(defn rand-vec
  ([n] (rand-vec n 1000 identity))
  ([n max] (rand-vec n max identity))
  ([n max caster]
     (->> #(rand-int max)
          repeatedly (map caster) (take n) vec)))

(defn rand-mtx
  ([n] (rand-mtx n 1000 identity))
  ([n max] (rand-mtx n max identity))
  ([n max caster]
     (letfn [(rand-row [] (->> #(rand-int max)
                               repeatedly (map caster) (take n) vec))]
       (->> rand-row repeatedly (take n) vec))))

;; # State

(def benches (atom {}))

;; # Benchmarks

(defn defbench-fun [method varying to-convert
                & bench-pairs]
  (let [bench-sets (->> bench-pairs
                        (partition 2)
                        (mapcat (fn [[k v :as pair]]
                                  (if (keyword? k)
                                    [pair]
                                    (map vector k (repeat v)))))
                        (map vec))]
    (swap! benches assoc method
           (assoc (into {} bench-sets)
             :varying varying
             :to-convert to-convert))))

(defmacro defbench [method varying to-convert & bench-pairs]
  (let [lazy-bench-pairs (->> bench-pairs
                              (partition 2)
                              (mapcat (fn [[k v]] [k `(delay ~v)]))
                              (concat))]
    `(defbench-fun ~method ~varying ~to-convert ~@lazy-bench-pairs)))

(defbench :construct-matrix
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) (rand-mtx 5)]
   :m [(rand-mtx 1) (rand-mtx 50)]
   :l [(rand-mtx 1) (rand-mtx 500)]})

(defbench :new-vector
  "size of constructed vector (5, 500, 500000)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) 5]
   :m [(rand-mtx 1) 500]
   :l [(rand-mtx 1) 500000]})

(defbench :new-matrix
  "size of constructed matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) 5   5]
   :m [(rand-mtx 1) 50  50]
   :l [(rand-mtx 1) 500 500]})

(defbench :new-matrix-nd
  "size of constructed 3d array (3, 20, 100)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) [3   3   3]]
   :m [(rand-mtx 1) [20  20  20]]
   :l [(rand-mtx 1) [100 100 100]]})

(defbench :dimensionality
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 3)]})

(defbench :get-shape
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 3)]})

(defbench :is-scalar?
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 3)]})

(defbench :is-vector?
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 3)]})

(defbench :dimension-count
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 3) 1]})

(defbench :clone
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :get-1d
  "1d vector size (5, 500, 50000)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)]
   :m [(rand-vec 500)   (rand-int 500)]
   :l [(rand-vec 50000) (rand-int 5000)]})

(defbench :get-2d
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500)]})

(defbench :get-nd
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)]]})

(defbench :set-1d
  "1d vector size (5, 500, 50000)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(defbench :set-1d!
  "1d vector size (5, 500, 50000)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d!
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd!
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(defbench :mutable-matrix
  "2d matrix size (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :new-scalar-array
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) 42]})

(defbench :get-0d
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [42]})

(defbench :set-0d!
  "nothing"
  #{0}
  [:ndarray :ndarray-double]
  {:s [42 13]})

(defbench :set-0d
  "nothing"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [42 13]})

(defbench :identity-matrix
  "size of requested matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) 5]
   :m [(rand-mtx 1) 50]
   :l [(rand-mtx 1) 500]})

(defbench :diagonal-matrix
  "size of requested matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) (rand-vec 5)]
   :m [(rand-mtx 1) (rand-vec 50)]
   :l [(rand-mtx 1) (rand-vec 500)]})

(defbench :coerce-param
  "size of coerced matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 1) (rand-mtx 5)]
   :m [(rand-mtx 1) (rand-mtx 50)]
   :l [(rand-mtx 1) (rand-mtx 500)]})

(defbench :broadcast
  "size of broadcasted vector (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5) [5 5]]
   :m [(rand-vec 50) [50 50]]
   :l [(rand-vec 500) [500 500]]})

(defbench :broadcast-like
  "size of broadcasted vector (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-vec 5)]
   :m [(rand-mtx 50)  (rand-vec 50)]
   :l [(rand-mtx 500) (rand-vec 500)]})

(defbench :convert-to-nested-vectors
  "size of converted matrix (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :reshape
  "size of matrix that is reshaped to vector (5, 50, 500)"
  #{0}
  [:vectorz]
  {:s [(rand-mtx 5)   [(* 5 5)]]
   :m [(rand-mtx 50)  [(* 50 50)]]
   :l [(rand-mtx 500) [(* 500 500)]]}
  [:vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(* 5 5)]]
   :m [(rand-mtx 50)  [(* 50 50)]]})

(defbench :get-row
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500)]})

(defbench :get-column
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500)]})

(defbench :get-major-slice
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500)]})

(defbench :get-slice
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 2) (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 2) (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 2) (rand-int 500)]})

(defbench :subvector
  "size of vector (5, 500, 50000)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 2)     (rand-int 3)]
   :m [(rand-vec 500)   (rand-int 200)   (rand-int 300)]
   :l [(rand-vec 50000) (rand-int 20000) (rand-int 30000)]})

(defbench :get-major-slice-view
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500)]})

(defbench :get-major-slice-seq
  "size of matrix that is sliced (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :join
  "size of matrices that are joined (5, 50, 500)"
  #{0 1}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]})

(defbench :main-diagonal
  "size of matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :assign!
  "size of matrix (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)    (rand-vec 5)]
   :m [(rand-mtx 50)   (rand-vec 50)]
   :l [(rand-mtx 500)  (rand-vec 500)]})

(defbench :fill!
  "size of matrix (5, 50, 500)"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)    42]
   :m [(rand-mtx 50)   42]
   :l [(rand-mtx 500)  42]})

(defbench :matrix-equals
  "size of matrix (5, 50, 500)"
  #{0 1}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s (let [m (rand-mtx 5)]   [m m])
   :m (let [m (rand-mtx 50)]  [m m])
   :l (let [m (rand-mtx 500)] [m m])})

(defbench :matrix-multiply
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:vecs]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :element-multiply
  "size of matrix (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)    2]
   :m [(rand-mtx 50)   2]
   :l [(rand-mtx 500)  2]})

(defbench :inner-product
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :outer-product
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)  (rand-mtx 5)]})

(defbench :add-product
  "2d matrix sizes (5, 50, 500)"
  #{0 1 2}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500) (rand-mtx 500)]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50) (rand-mtx 50)]})

(defbench :add-product!
  "2d matrix sizes (5, 50, 500)"
  #{0 1 2}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500) (rand-mtx 500)]}
  [:ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50) (rand-mtx 50)]})

(defbench :add-scaled-product
  "2d matrix sizes (5, 50, 500)"
  #{0 1 2}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   (rand-mtx 5)   42]
   :m [(rand-mtx 50)  (rand-mtx 50)  (rand-mtx 50)  42]
   :l [(rand-mtx 500) (rand-mtx 500) (rand-mtx 500) 42]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  (rand-mtx 5)  42]
   :m [(rand-mtx 50) (rand-mtx 50) (rand-mtx 50) 42]})

(defbench :add-scaled-product!
  "2d matrix sizes (5, 50, 500)"
  #{0 1 2}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   (rand-mtx 5)   42]
   :m [(rand-mtx 50)  (rand-mtx 50)  (rand-mtx 50)  42]
   :l [(rand-mtx 500) (rand-mtx 500) (rand-mtx 500) 42]}
  [:ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  (rand-mtx 5)  42]
   :m [(rand-mtx 50) (rand-mtx 50) (rand-mtx 50) 42]})

(defbench :add-scaled
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   42]
   :m [(rand-mtx 50)  (rand-mtx 50)  42]
   :l [(rand-mtx 500) (rand-mtx 500) 42]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  42]
   :m [(rand-mtx 50) (rand-mtx 50) 42]})

(defbench :add-scaled!
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)   42]
   :m [(rand-mtx 50)  (rand-mtx 50)  42]
   :l [(rand-mtx 500) (rand-mtx 500) 42]}
  [:ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)  42]
   :m [(rand-mtx 50) (rand-mtx 50) 42]})

(defbench :element-divide
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   42]
   :m [(rand-mtx 50)  42]
   :l [(rand-mtx 500) 42]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  42]
   :m [(rand-mtx 50) 42]})

(defbench :matrix-multiply!
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :element-multiply!
  "size of matrix (5, 50, 500)"
  #{0}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)    2]
   :m [(rand-mtx 50)   2]
   :l [(rand-mtx 500)  2]})

(defbench :scale
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   42]
   :m [(rand-mtx 50)  42]
   :l [(rand-mtx 500) 42]})

(defbench :pre-scale
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   42]
   :m [(rand-mtx 50)  42]
   :l [(rand-mtx 500) 42]})

(defbench :scale!
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   42]
   :m [(rand-mtx 50)  42]
   :l [(rand-mtx 500) 42]})

(defbench :pre-scale!
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   42]
   :m [(rand-mtx 50)  42]
   :l [(rand-mtx 500) 42]})

(defbench :matrix-add
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]})

(defbench :matrix-sub
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]})

(defbench :matrix-add!
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]})

(defbench :matrix-sub!
  "2d matrix sizes (5, 50, 500)"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :transpose
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :numerical?
  "2d matrix sizes (5, 50, 500)"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})
