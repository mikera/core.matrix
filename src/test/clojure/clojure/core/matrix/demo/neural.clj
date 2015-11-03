(ns clojure.core.matrix.demo.neural
  "Demonstration of a simple neural network with logistic activation functions using core.matrix"
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [error]]))

;; first we construct a map that defines our initial neural network state
;; we populate this with random gaussian values
(def INITIAL-NETWORK
  (let [r (java.util.Random.)
        
        ;; the structure, defined as the number of nodes in each layer (input, hidden, output)
        structure [4 5 1]
        
        ;; weight matrixes between each later
        weights (mapv (fn [n m] (reshape (repeatedly #(.nextGaussian r)) [n m])) (next structure) (butlast structure))
        
        ;; bias weights for each output
        biases (mapv (fn [n] (reshape (repeatedly #(.nextGaussian r)) [n])) (next structure))]
  {:structure structure
   :weights weights
   :biases biases}))

(defn think 
  "Makes the neural network think with a given input vector, creating activations at each layer" 
  [net input]
  (assoc net :activations
         (vec (reductions
                (fn [x [weight bias]]
                  (logistic (add (mmul weight x) bias)))
                input
                (map vector (:weights net) (:biases net))))))

(defn compute-error
  "Computes error gradient for weights and biases, given a target vector"
  ([net target]
    (when-not (:activations net) (error "Must call think on net first to create activations"))
    (let [output (last (:activations net))
          N (count (:structure net))
          g (mul (sub target output) 2)
          net (let [empty-vec (vec (repeat (dec N) nil))]
                (assoc net 
                       :gradient-weights empty-vec
                       :gradient-biases empty-vec
                       :gradient (conj empty-vec g)))]
      (loop [i (- N 2) ;; start at layer before output
             net net] 
        (if (< i 0)
          net
          (let [gradient (nth (:gradient net) (inc i))
                weight (nth (:weights net) i)
                bias (nth (:biases net) i)
                input (nth (:activations net) i)
                output (nth (:activations net) (inc i))
                deriv (mul output (sub 1.0 output))
                gderiv (mul gradient deriv)]
            (recur (dec i)
                   (-> net 
                     (assoc-in [:gradient-biases i] gderiv)
                     (assoc-in [:gradient-weights i] (outer-product gderiv input))
                     (assoc-in [:gradient i] (mmul (transpose weight) gderiv))))))))))

(defn param-update
  "Updates the neural network parameters by gradient descent, scaling the updates by a given factor."
  [net factor]
  (reduce 
    (fn [net i]
      (-> net
        (update-in [:weights i] (fn [wt] (add wt (mul factor (get-in net [:gradient-weights i])))))
        (update-in [:biases i] (fn [bs] (add bs (mul factor (get-in net [:gradient-biases i])))))))
    net
    (range (dec (count (:structure net))))))

(defn train
  "Trains a network for n iterations, with a collection of input -> target pairs"
  [net examples n]
  (let [examples (vec examples)]
    (loop [i 0
           net net]
      (if (>= i n)
        net
        (let [example (rand-nth examples)
              input (first example)
              target (second example)
              net (think net input)
              net (compute-error net target)
              learn-rate (/ 1.0 (+ 1.0 (* 0.1 (/ i n)))) ;; decreasing learning rate
              net (param-update net learn-rate) 
              ]
          (recur 
            (inc i) 
            (update-in net [:train-count] (fnil inc 0))))))))

(def EXAMPLES 
  [[[1 0 0 0] [1]]
   [[0 1 0 0] [0]]
   [[0 1 0 0] [1]]
   [[0 1 1 0] [0]]
   [[0 1 1 1] [1]]
   [[1 0 0 1] [0]]])

(defn classifier 
  "Creates a classifier function from a trained neural network" 
  [net]
  (fn [x]
    (last (:activations (think net x)))))

(comment ;; code to run at REPL
  
  ;; train the network with randomly drawn examples
  (def NET (train INITIAL-NETWORK EXAMPLES 10000))

  ;; create a classifier using the new net
  (def f (classifier NET))
  
  ;; test on some examples
  (f [1 0 0 0]) ;; => [0.9821892418636621]  
  (f [1 0 0 1]) ;; => [0.015063456928232077]
  )
