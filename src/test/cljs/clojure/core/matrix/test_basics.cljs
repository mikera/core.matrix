(ns clojure.core.matrix.test-basics
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [clojure.core.matrix :as mat]))

(enable-console-print!)

(defn sum-two-numbers
  "Returns sum of two numbers"
  [x y]
  (+ x y))

(deftest test-matrix-math
  (let [a (mat/array [1 2 3])
        b (mat/array [1 2 3])
        c (mat/array [[2 2] [2 2]])
        d (mat/array [[3 3] [3 3]])]
    (is (= [2 4 6] (vec (mat/add a b))))
    (is (= [1 4 9] (vec (mat/mul a b))))
    ;(println (vec (map vec (mat/mmul c d))))
    ))

(defn set-html! [el content]
  (set! (.-innerHTML el) content))

(defn ^:export init []
  (let [element (.getElementById js/document "app")
        content (str "2 + 2 = " (sum-two-numbers 2 2))]
    (test-matrix-math)
    (set-html! element content)))
