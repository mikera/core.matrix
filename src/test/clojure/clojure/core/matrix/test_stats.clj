(ns clojure.core.matrix.test-stats
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.stats)
  (:use clojure.test))

(deftest test-mean
  (is (equals [1 2] (mean [[0 0] [1 2] [2 4]])))
  (is (== 1.5 (mean [0 1 2 3]))))

(deftest test-sum
  (is (== 6 (first (slices [6]))))
  (is (nil? (next (slices [6]))))
  (is (== 6 (sum [6])))
  (is (== 6 (sum [1 2 3])))
  (let [ss (sum [[1 1] [1 1] [1 2]])]
    (is (equals [3 4] ss))
    (is (mutable? ss))))

(deftest test-sum-of-squares
  (is (== 13 (sum-of-squares [2 3])))
  (is (== 30 (sum-of-squares [1 2 3 4])))
  (let [ss (sum-of-squares [[-1 1] [1 2] [2 3] [3 4]])]
    (is (equals [15 30] ss))
    (is (mutable? ss))))

(deftest test-variance
  (is (== 0.0 (variance [2 2])))
  (is (== 0.0 (variance [1 1 1])))
  (is (== 2.5 (variance [1 2 3 4 5]))))

(deftest test-sd
  (is (== 0.0 (sd [1 1 1]))))

(deftest test-normalise-probabilities
  (is (equals [0.5 0.5] (normalise-probabilities [10 10])))
  (is (equals [1.0] (normalise-probabilities [0])))
  (is (equals [0.5 0.5] (normalise-probabilities [0 0])))
  (is (equals [0.25 0.75] (normalise-probabilities [1 3]))))

(defn approx
  "returns true if expected and actual are within tolerance of each other"
  ([exp actual] (approx exp actual 0.05))
  ([exp actual tolerance]
   (> tolerance  (Math/abs  (- exp actual)))))

(deftest test-correlation
  (is (approx 0.5298
              (let [x [43 21 25 42 57 59]
                    y [99 65 79 75 87 81]]
                (correlation x y)))))

(deftest test-r-squared
  (is (= 1.0 (r-squared [1 2 3] [1 2 3])))
  (is (= 1.0 (r-squared [1 nil 3] [1 nil 3] true)))
  (is (= 0.0 (r-squared [1 2 3] [2 2 2])))
  (is (nil? (r-squared ["" 2] [1 ""] true))))

(deftest test-rmse
  (is (approx 2.915
              (rmse [7 10 12 10 10 8 7 8 11 13 10 8]
                    [6 10 14 16 7 5 5 13 12 13 8 5]))))

(deftest test-cosine-similarity
  (is (approx 0.93857
              (cosine-similarity [2 4 3 1 6]
                                 [3 5 1 2 5]))))
