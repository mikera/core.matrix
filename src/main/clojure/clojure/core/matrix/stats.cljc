(ns clojure.core.matrix.stats
  "Namespace for statistical functions operating on core.matrix arrays.

   Previously some of these functions were available in a separate `core.matrix.stats` library,
   they have now been fully integrated into core.matrix."
  (:require [clojure.core.matrix :as m]))

(defn sum
  "Calculates the sum of a collection of values.
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (if (== 1 (m/dimensionality values))
      (m/esum values)
      (let [values (m/slices values)
            result (m/mutable (first values))]
        (doseq [v (next values)]
          (m/add! result v))
        result))))

(defn sum-of-squares
  "Calculates the sum of squares of a collection of values.
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (if (== 1 (m/dimensionality values))
      (m/inner-product values values)
      (let [values (m/slices values)
            fv (first values)
            result (m/mutable (m/mul fv fv))]
        (doseq [v (next values)]
          (m/add! result (m/mul v v))
          ;; (add-product! result v v)
        ) ;; TODO: convert to add-product! when fixed in core.matrix NDArray
        result))))

(defn mean
  "Calculates the mean of a collection of values.
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (let [values (m/slices values)
          n (m/dimension-count values 0)
          s (sum values)]
      (if (number? s)
        (/ s n)
        (m/scale! s (/ 1.0 n)) ;; abuse the fact that s must be a new mutable matrix....
        ))))

(defn variance
   "Calculates the unbiased sample variance of a set of values.
   Values may be scalars, vectors or higher-dimensional matrices."
   ([values]
     (let [n (m/dimension-count values 0)
           u (mean values)
           ss (sum-of-squares values)
           nuu (m/mul n (m/emul u u))]
       (if (number? ss)
         (* (- ss nuu) (/ 1.0 (dec n)))
         (do ;; must be a new mutable matrix, so we abuse this fact to use it as an accumulator...
           (m/sub! ss nuu)
           (m/scale! ss (/ 1.0 (dec n)))
           ss)))))

(defn sd
  "Calculates the sample standard deviation of a set of values.
   Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
    (Math/sqrt (variance values))))

(defn normalise-probabilities
  "Normalises a numerical probability vector, i.e. to a vector where all elements sum to 1.0.

   A zero vector will be set set to [1/n .... 1/n]."
  ([v]
    (let [len (double (m/esum v))]
      (cond
        (== len 1.0) v
        (== len 0.0) (m/assign v (/ 1.0 (m/dimension-count v 0)))
        :else (m/scale v (/ 1.0 len))))))

