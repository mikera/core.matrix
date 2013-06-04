(defn swap 
  "Swap row i with row j"
  [X i j] 
  (assoc X j (X i) i (X j)))

(defn multiply
  "Multiply row i by constant k"
  [X i k]
  (assoc X i (* k (X i))))

(defn add
  "Add a row j times a constant k to a row i and replace i"
  [X i j k]
  (assoc X i (+ (X i) (* k (X j)))))

