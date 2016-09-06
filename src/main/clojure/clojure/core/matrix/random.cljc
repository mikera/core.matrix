(ns clojure.core.matrix.random
  "Namespace for random number generation functions working with core.matrix
   arrays.

   Intended for use in random sampling, simulation etc."
  (:require [clojure.core.matrix :as m])
#?(:clj (:import [java.util Random]
                 [clojure.core.matrix.random RandomSeq])))

#?(:clj (do
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
))

#?(:clj
(defn- to-random
  "Returns a java.util.Random instance. May be used as seed for random
   sampling functions.

   An optional seed value may be provided to ensure a repeatable random sequence.
   The seed may be anything (number, string arbitrary object etc.): its hashcode
   may be used to generate a long seed."
  (^Random []
    (java.util.Random.))
  (^Random [seed]
    (cond
      (instance? Random seed) seed
      (number? seed) (java.util.Random. (long seed))
      :else (java.util.Random. (long (.hashCode ^Object seed))))))

:cljs (defn- to-random [& _] nil))

; This function implements the Kinderman-Monahan ratio method:
;  A.J. Kinderman & J.F. Monahan
;  Computer Generation of Random Variables Using the Ratio of Uniform Deviates
;  ACM Transactions on Mathematical Software 3(3) 257-260, 1977
; Extracted from clojure.contrib monte-carlo by Konrad Hinsen
(defn rand-gaussian
  ([] (rand-gaussian 0.0 1.0))
  ([mu sigma]
    (let [u1  (rand)
          u2* (rand)
          u2 (- 1.0 (double u2*))
          s (* 4 (/ #?(:clj (Math/exp -0.5) :cljs (.exp js/Math -0.5)) #?(:clj (Math/sqrt 2.0) :cljs (.sqrt js/Math 2.0))))
          z (* s (/ (- (double u1) 0.5) u2))
          zz (+ (* 0.25 z z) #?(:clj (Math/log u2) :cljs (.log js/Math u2)))]
      (if (> zz 0)
        (recur mu sigma)
        (+ (double mu) (* (double sigma) z))))))

(defn randoms
  "Returns an infinite lazy sequence of random samples from a uniform distribution on [0,1).

   May be given a optional seed that is either an integer value or a java.util.Random instance"
  ([]
   #?(:clj (RandomSeq. (java.util.Random.))
      :cljs (repeatedly rand)))
  ([seed]
   #?(:clj (RandomSeq. (to-random seed))
      :cljs (repeatedly rand))))

(defn sample-uniform
  "Returns an array of random samples from a uniform distribution on [0,1).

   Size may be either a number of samples or a shape vector."
  ([size]
    (sample-uniform size (to-random)))
  ([size seed]
    (let [r (to-random seed)
          size (if (number? size) [size] size)]
      (m/compute-matrix
        size
        (fn [& ixs]
          #?(:clj (.nextDouble r)
             :cljs (rand)))))))

(defn sample-normal
  "Returns an array of random samples from a standard normal distribution.

   Size may be either a number of samples or a shape vector."
  ([size]
    (sample-normal size (to-random)))
  ([size seed]
    (let [size (if (number? size) [size] size)
          r (to-random seed)]
      (m/compute-matrix
        size
        (fn [& ixs]
          #?(:clj (.nextGaussian r)
             :cljs (rand)))))))

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
      (m/compute-matrix
        size
        (fn [& ixs]
          (long (* n #?(:clj (.nextDouble r) :cljs (rand)))))))))

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
      (m/compute-matrix
        size
        (fn [& ixs]
          (loop [i 0 , res 0]
            (if (< i n)
              (recur (inc i) (if (< #?(:clj (.nextDouble r) :cljs (rand)) p) (inc res) res))
              res)))))))
