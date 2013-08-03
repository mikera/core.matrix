(ns clojure.core.matrix.demo.gravity
  (:use clojure.core.matrix)
  (:import [javax.swing JFrame JPanel JComponent Timer])
  (:import [java.awt Dimension Graphics Color])
  (:import [java.awt.event ActionListener ActionEvent]))

;; model is a 2D array of [colour radius position velocity mass] for each object
(defn new-model [n]
  (vec (for [i (range n)]
                    (let [radius (rand)]
                      [(rand-int 16777216) 
                       radius 
                       (vec (repeatedly 3 #(+ 0.3 (* (rand) 0.4)))) 
                       (vec (repeatedly 3 #(* 0.2 (- (rand) 0.5)))) 
                       (* radius radius radius)]))))

;; a gravitational constant
(def G 0.003)

;; effective max radius
(def R 0.05)

(defn update [model time]
  (let [time (double time)
        n (dimension-count model 0)
        positions (slice model 1 2)
        masses (slice model 1 4)
        calc-force-vector (fn [pos] ;; calculate the gravity field at a point in space 
                            (let [offsets (sub positions pos)
                                  dists (mapv length offsets)
                                  DR (* 2.0 R)]
                              (reduce (fn [acc i] 
                                        (let [dist (double (max DR (mget dists i)))]
                                          (add acc (mul G (mget masses i) (/ 1.0 dist dist dist) (mget offsets i))))) 
                                      [0 0 0] 
                                      (range n))))]
    (mapv (fn [[colour radius position velocity mass]]
            (let [new-pos (add-scaled position velocity time)
                  force (mul mass (calc-force-vector position))  
                  new-vel (add-scaled velocity force (/ time mass))]
              [colour radius new-pos new-vel mass]))
      model)))

(defn demo []
  (let [model (atom (new-model 30))
        ^JComponent comp (proxy [JComponent] []
                           (paintComponent [^Graphics g]
                             (let [h (int (.getHeight this))
                                   w (int (.getWidth this))
                                   m (int (min h w))]
                               (.setColor g (Color/BLACK))
                               (.fillRect g 0 0 w h)    
                               (doseq [[colour radius pos] @model]
                                 (let [x (double (mget pos 0))
                                       y (double (mget pos 1))
                                       r (* R (double radius))
                                       hr (* 0.5 r)]
                                   (.setColor g (Color. (int colour)))
                                   (.fillOval g
                                     (int (* m (- x hr)))
                                     (int (* m (- y hr)))
                                     (* m r) (* m r)))))))
        ^JFrame frame (JFrame. "Gravity demo")
        ^ActionListener anim (proxy [ActionListener] []
                           (actionPerformed [^ActionEvent e]                          
                             (when (.isShowing comp)
                               (swap! model update 0.02)
                                (.repaint comp))))]
    (.setPreferredSize comp (Dimension. 700 700))
    (.add (.getContentPane frame) comp)
    (.start (Timer. 50 anim)) 
    (.pack frame)
    (.setVisible frame true))
  )


