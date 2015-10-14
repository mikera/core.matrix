(ns clojure.core.matrix.random
  (:use clojure.core.matrix)
  (:use [mikera.cljutils error])
  (:import [java.util Random])
  (:import [clojure.core.matrix.random RandomSeq]))

(defn to-random ^Random [seed]
  (cond 
    (instance? Random seed) seed
    (number? seed) (java.util.Random. seed)
    :else (error "Can't convert to Random instance: " seed)))

(defn randoms 
  "Returns a lazy sequence of random numbers, given a seed that is either an integer value or a java.util.Random instance"
  ([]
    (randoms (System/currentTimeMillis)))
  ([seed]
    (let [^Random rnd (to-random seed)]
      (RandomSeq. rnd))))

(defn sample-uniform 
  "Returns an array of random samples from a uniform distribution on [0,1)

   Size may be either a number of samples or a shape vector."
  ([size]
    (let [size (if (number? size) [size] size)]
      (compute-matrix 
        size
        (fn [& ixs]
          (Math/random))))))

(defn sample-normal 
  "Returns an array of random samples from a standard normal distribution.

   Size may be either a number of samples or a shape vector."
  ([size]
    (let [size (if (number? size) [size] size)
          r (java.util.Random.)]
      (compute-matrix 
        size
        (fn [& ixs]
          (.nextGaussian r))))))

(defn sample-rand-int 
  "Returns an array of random integers in the range [0..n), equivalent to
   Clojure's rand-int function.

   Size may be either a number of samples or a shape vector."
  ([size n]
    (let [size (if (number? size) [size] size)
          r (java.util.Random.)]
      (compute-matrix 
        size
        (fn [& ixs]
          (rand-int n))))))

;; TODO: should use normal approximation to binomial for large n
(defn sample-binomial 
  "Returns an array of samples from a binomial distribution with probability p 
   and n trials for each sample. If n is omitted, a single trial is performed.

   Size may be either a number of samples or a shape vector."
  ([size p]
    (sample-binomial size p 1))
  ([size p n]
    (let [size (if (number? size) [size] size)
          r (java.util.Random.)
          n (long n)
          p (double p)]
      (compute-matrix 
        size
        (fn [& ixs]
          (loop [i 0 , res 0]
            (if (< i n)
              (recur (inc i) (if (<= (Math/random) p) (inc res) res))
              res)))))))