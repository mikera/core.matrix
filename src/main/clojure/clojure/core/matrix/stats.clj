(ns clojure.core.matrix.stats
  (:use clojure.core.matrix))

(defn sum 
  "Calculates the sum of a collection of values. 
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (if (== 1 (dimensionality values))
      (esum values)
      (let [values (slices values)
            result (mutable (first values))]
        (doseq [v (next values)]
          (add! result v))
        result))))

(defn sum-of-squares
  "Calculates the sum of squares of a collection of values. 
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (if (== 1 (dimensionality values))
      (inner-product values values)
      (let [values (slices values)
            fv (first values) 
            result (mutable (mul fv fv))]
        (doseq [v (next values)]
          (add! result (mul v v))
          ;; (add-product! result v v)
        ) ;; TODO: convert to add-product! when fixed in core.matrix NDArray
        result))))

(defn mean
  "Calculates the mean of a collection of values. 
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (let [values (slices values)
          n (dimension-count values 0)
          s (sum values)]
      (if (number? s)
        (/ s n)
        (scale! s (/ 1.0 n)) ;; abuse the fact that s must be a new mutable matrix....
        ))))

(defn variance
   "Calculates the unbiased sample variance of a set of values.
   Values may be scalars, vectors or higher-dimensional matrices."
   ([values]
     (let [n (dimension-count values 0)
           u (mean values)
           ss (sum-of-squares values)
           nuu (mul n (emul u u))]
       (if (number? ss)
         (* (- ss nuu) (/ 1.0 (dec n)))
         (do ;; must be a new mutable matrix, so we abuse this fact to use it as an accumulator...
           (sub! ss nuu) 
           (scale! ss (/ 1.0 (dec n)))
           ss)))))

(defn sd
  "Calculates the sample standard deviation of a set of values.
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (sqrt (variance values))))

(defn normalise-probabilities
  "Normalises a numerical probability vector, i.e. to a vector where all elements sum to 1.0.
   A zero vector will be set set to [1/n .... 1/n]."
  ([v]
    (let [len (double (sum v))]
      (cond
        (== len 1.0) v
        (== len 0.0) (coerce v (let [n (dimension-count v 0)] (repeat n (/ 1.0 n))))
        :else (scale v (/ 1.0 len))))))

