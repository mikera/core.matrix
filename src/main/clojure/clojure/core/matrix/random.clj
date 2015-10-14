(ns clojure.core.matrix.random
  "Namespace for random number generation functions working with core.matrix
   arrays. Intended for use in rnadom sampling, simulation etc."
  (:use clojure.core.matrix)
  (:use [mikera.cljutils error])
  (:import [java.util Random])
  (:import [clojure.core.matrix.random RandomSeq]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn to-random 
  "Returns a java.util.Random instance. May be used as seed for random 
   sampling functions. 

   An option seed may be provided to ensure a repeatable random sequence"
  (^Random []
    (java.util.Random.))
  (^Random [seed]
    (cond 
      (instance? Random seed) seed
      (number? seed) (java.util.Random. (long seed))
      :else (error "Can't convert to Random instance: " seed))))

(defn random-seq
  "Returns a seq of random double values uniformaly distributed in the range [0,1)"
  ([]
    (RandomSeq. (java.util.Random.)))
  ([seed]
    (RandomSeq. (to-random seed))))

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
    (sample-uniform size (to-random)))
  ([size seed]
    (let [rnd (to-random seed)
          size (if (number? size) [size] size)]
      (compute-matrix 
        size
        (fn [& ixs]
          (.nextDouble rnd))))))

(defn sample-normal 
  "Returns an array of random samples from a standard normal distribution.

   Size may be either a number of samples or a shape vector."
  ([size]
    (sample-normal size (to-random)))
  ([size seed]
    (let [size (if (number? size) [size] size)
          r (to-random seed)]
      (compute-matrix 
        size
        (fn [& ixs]
          (.nextGaussian r))))))

(defn sample-rand-int 
  "Returns an array of random integers in the range [0..n), equivalent to
   Clojure's rand-int function.

   Size may be either a number of samples or a shape vector."
  ([size n]
    (sample-rand-int size n (to-random)))
  ([size n seed]
    (let [size (if (number? size) [size] size)
          n (double n)
          r (to-random seed)]
      (compute-matrix 
        size
        (fn [& ixs]
          (long (* n (.nextDouble r))))))))

;; TODO: should use normal approximation to binomial for large n
(defn sample-binomial 
  "Returns an array of samples from a binomial distribution with probability p 
   and n trials for each sample. If n is omitted, a single trial is performed.

   Size may be either a number of samples or a shape vector."
  ([size p]
    (sample-binomial size p 1))
  ([size p n]
    (sample-binomial size p n (to-random)))
  ([size p n seed]
    (let [size (if (number? size) [size] size)
          r (to-random seed)
          n (long n)
          p (double p)]
      (compute-matrix 
        size
        (fn [& ixs]
          (loop [i 0 , res 0]
            (if (< i n)
              (recur (inc i) (if (< (.nextDouble r) p) (inc res) res))
              res)))))))