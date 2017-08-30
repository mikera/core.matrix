(ns clojure.core.matrix.stats
  "Namespace for statistical functions operating on core.matrix arrays.

   Previously some of these functions were available in a separate `core.matrix.stats` library,
   they have now been fully integrated into core.matrix."
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lm]))

(defn sum
  "Calculates the sum of a collection of values.
   Values may be:
    - A vector, in which case the result is a single scalar
    - A higher-dimensional array, in which case the result is the sum of all slices."
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
   Values may be:
   - A vector, in which case the result is a single scalar
   - A higher-dimensional array, in which case the result is the sum of squares in all slices."
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
   Values may be:
   - A vector, in which case the result is a single scalar
   - A higher-dimensional array, in which case the result is the mean of all slices."
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
         nuu (m/mul n (m/mul u u))]
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
   (m/sqrt (variance values))))

(defn normalise-probabilities
  "Normalises a numerical probability vector, i.e. to a vector where all elements sum to 1.0.

   A zero vector will be set set to [1/n .... 1/n]."
  ([v]
   (let [len (double (m/esum v))]
     (cond
       (== len 1.0) v
       (== len 0.0) (m/assign v (/ 1.0 (m/dimension-count v 0)))
       :else (m/scale v (/ 1.0 len))))))

(defn correlation
  "Returns the Pearson correlation coefficient"
  [x y]
  (let [x2 (m/square x)
        y2 (m/square y)
        n (count x)
        xy (m/mul x y)
        sx (m/esum x)
        sy (m/esum y)]
    (double (/ (-  (* n (m/esum xy)) (* sx sy))
               (m/sqrt (* (- (* n (m/esum x2)) (m/square sx))
                          (- (* n (m/esum y2)) (m/square sy))))))))

(defn filter-nils
  "returns actl and pred vectors where both pairs are valid numbers and are non-nil"
  [actl pred]
  (let [iseq (filter (fn [[a b]] (and (number? a) (number? b)))
                      (map vector pred actl))
        v1 (mapv first iseq)
        v2 (mapv second iseq)]
     (vector v2 v1)))

(defn r-squared
  "Returns the coefficient of determination(R-squared) between 2 vectors, If filter-nils? is true, the pairs are filtered to have  
   numerical values. Returns nil if there are no corresponding values or if the result is a NaN"
  ([actl pred filter-nils?]
   (let [[v2 v1] (if filter-nils? (filter-nils actl pred)
                   [actl pred])]
     (r-squared v2 v1)))
  ([actl pred]
   (let [sse (-> (m/sub actl pred) m/square m/esum double)]
     (if (> (count pred) 0)
       (let [ymean (mean actl)
             ydiff (m/sub actl ymean)
             tss (double (m/esum (m/square ydiff)))
             res (- 1.0 (/ sse tss))]
         (if (or  (= res Double/NEGATIVE_INFINITY) (Float/isNaN res))
           nil res))
       nil))))

(defn rmse
  "returns RMSE between 2 vectors."
  [v1 v2]
  (let  [n  (count v1)
         diff  (m/sub v1 v2)
         diff2  (m/square diff)
         sse  (m/esum diff2)]
    (when  (> n 0)  (Math/sqrt  (double (/ sse n))))))

(defn cosine-similarity
  "returns the cosine similarity between 2 vectors "
  [v1 v2]
  (/ (m/dot v1 v2) (* (lm/norm v1) (lm/norm v2))))
