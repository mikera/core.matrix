(ns clojure.core.matrix.demo.neural
  "Demonstration of a simple N-layer neural network with logistic activation functions using core.matrix
   We use an immutable neural network structure to demonstate idiomatic Clojure style"
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.random :refer [sample-normal]]
            [clojure.core.matrix.utils :refer [error]]))

;; OPTIONAL: choose a core.matrix implementation (for better performance)
;; (set-current-implementation :vectorz)

;; =================================================================================================
;; First we construct our initial neural network state
;; we populate this with random gaussian values using clojure.core.matrix.random
;;
;; We make sure of Clojure' persistent maps to store our 
;; neural netowrk as a single, immutable data structure
(def INITIAL-NETWORK
  (let [;; The structure, defined as the number of nodes in each layer (input, hidden, output)
        structure [4 5 1]
        
        ;; Create weight matrixes between each layer
        weights (mapv (fn [n m] (sample-normal [n m])) 
                      (next structure) 
                      (butlast structure))
        
        ;; Bias weights for each non-input layer
        biases (mapv (fn [n] (sample-normal [n])) 
                     (next structure))]
  {:structure structure
   :weights weights
   :biases biases}))

;; =================================================================================================
;; Nueual network computation and training functions

;; the `think` function computes activations for each layer, starting with the input
;; we use `reductions` as a handy way to do this
(defn think 
  "Makes the neural network 'think' with a given input vector, computing activations at each layer.
   Activations are assoc'd into the neural network." 
  [net input]
  (assoc net :activations
         (vec (reductions
                (fn [x [weight bias]]
                  (logistic (add (mmul weight x) bias))) ;; Layer output Y = logistic(W.X + B)
                (array input)
                (map vector (:weights net) (:biases net))))))


(defn compute-error
  "Computes error gradient for weights and biases, given a target vector.
   Error gradients are assoc'd into the neural network."
  ([net target]
    (when-not (:activations net) (error "Must call think on net first to create activations"))
    (let [output (last (:activations net))
          N (count (:structure net))
          g (mul (sub (array target) output) 2) ;; Error on output layer G = 2 * (T - Y)
          
          ;; initialise vectors to store the gradients
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
                     (assoc-in [:gradient-biases  i] gderiv)                      
                     (assoc-in [:gradient-weights i] (outer-product gderiv input)) 
                     (assoc-in [:gradient i] (mmul (transpose weight) gderiv))))))))))


;; parameter update simply adds a multiple of the error gradient to every parameter in the network
;; (both weight matrices and bias vectors for each layer). Note that the core.matrix code for weights
;; and biases is the same because differences in diemnsionality are handled automatically.
(defn param-update
  "Updates the neural network parameters by gradient descent, scaling the updates by a given factor.
   Weights and biases in all layers are updated in the resulting network."
  [net factor]
  (reduce 
    (fn [net i]
      (-> net
        (update-in [:weights i] (fn [wt] (add wt (mul factor (get-in net [:gradient-weights i])))))
        (update-in [:biases i]  (fn [bs] (add bs (mul factor (get-in net [:gradient-biases  i])))))))
    net
    (range (dec (count (:structure net))))))


;; Training function repeatedly runs the network against randomly chosen examples and updates parameters
;; according to the gradient. Note that this is a fairly naive approach, a better approach would:
;; - iterate over entire example set or mini-batches to compute each parameter update
;; - use momentum or some accelerated method of gradient descent (ADAGRAD etc.)
(defn train
  "Trains a network for n iterations, with a collection of input -> target pairs"
  [net examples n]
  (let [examples (vec examples)]
    (loop [i 0
           net net]
      (if (>= i n)
        net
        (let [example (rand-nth examples) ;; choose a random example
              input (first example)       ;; get the example input
              target (second example)     ;; get the example (target) output)
              net (think net input)
              net (compute-error net target)
              learn-rate (/ 1.0 (+ 1.0 (* 0.1 (/ i n)))) ;; decreasing learning rate
              net (param-update net learn-rate) 
              ]
          (recur 
            (inc i) 
            (update-in net [:train-count] (fnil inc 0))))))))

;; Example training data, as a collection of input -> target pairs
(def EXAMPLES 
  [[[1 0 0 0] [1]]
   [[0 1 0 0] [0]]
   [[0 1 0 0] [1]]
   [[0 1 1 0] [0]]
   [[0 1 1 1] [1]]
   [[1 0 0 1] [0]]])


;; Higher order function to create a classifier function from a neural network. This 
;; effectively does the following:
;;  - run `think` on the neural network with the given input
;;  - extract the activations that represent the top-level neural network output
(defn classifier 
  "Creates a classifier function from a trained neural network" 
  [net]
  (fn [x]
    (last (:activations (think net x)))))

;; =================================================================================================
;; Code to run at REPL
(comment 
  
  ;; train the network with randomly drawn examples
  (def NET (train INITIAL-NETWORK EXAMPLES 10000))

  ;; create a classifier using the new net
  (def f (classifier NET))
  
  ;; test on some examples
  (f [1 0 0 0]) ;; => [0.9821892418636621]  
  (f [1 0 0 1]) ;; => [0.015063456928232077]
  )
