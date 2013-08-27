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

(defn defbench [method varying to-convert
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

(defbench :matrix-multiply
  "matrix sizes"
  #{0 1}
  [:vectorz :ndarray-double]
  {:s [(rand-mtx 5)   (rand-mtx 5)]
   :m [(rand-mtx 50)  (rand-mtx 50)]
   :l [(rand-mtx 500) (rand-mtx 500)]}
  [:vecs :ndarray]
  {:s [(rand-mtx 5)  (rand-mtx 5)]
   :m [(rand-mtx 50) (rand-mtx 50)]})

(defbench :clone
  "2d matrix sizes"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)]
   :m [(rand-mtx 50)]
   :l [(rand-mtx 500)]})

(defbench :get-1d
  "1d vector size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)]
   :m [(rand-vec 500)   (rand-int 500)]
   :l [(rand-vec 50000) (rand-int 5000)]})

(defbench :get-2d
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500)]})

(defbench :get-nd
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)]]})

(defbench :set-1d
  "1d vector size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd
  "2d matrix size"
  #{0}
  [:vectorz :vecs :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(defbench :set-1d!
  "1d vector size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-vec 5)     (rand-int 5)    (rand-int 1000)]
   :m [(rand-vec 500)   (rand-int 500)  (rand-int 1000)]
   :l [(rand-vec 50000) (rand-int 5000) (rand-int 1000)]})

(defbench :set-2d!
  "2d matrix size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   (rand-int 5)   (rand-int 5)   (rand-int 1000)]
   :m [(rand-mtx 50)  (rand-int 50)  (rand-int 50)  (rand-int 1000)]
   :l [(rand-mtx 500) (rand-int 500) (rand-int 500) (rand-int 1000)]})

(defbench :set-nd!
  "2d matrix size"
  #{0}
  [:vectorz :ndarray :ndarray-double]
  {:s [(rand-mtx 5)   [(rand-int 5)   (rand-int 5)]   (rand-int 1000)]
   :m [(rand-mtx 50)  [(rand-int 50)  (rand-int 50)]  (rand-int 1000)]
   :l [(rand-mtx 500) [(rand-int 500) (rand-int 500)] (rand-int 1000)]})

(def noop nil)