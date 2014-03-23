(ns clojure.core.matrix.test-generic
  (:use clojure.test)
  (:require [clojure.core.matrix :as cm])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.generic :refer :all])
  (:require [clojure.core.matrix.generic-protocols :as gmp])
  (:require [simple-check.core :as sc])
  (:require [simple-check.generators :as gen])
  (:require [simple-check.properties :as prop])
  (:require [simple-check.clojure-test :as ct :refer (defspec)])
  (:import [clojure.core.matrix.generic]))

(def real (map->Specialisation {:add +
                                :mul *
                                :sub -
                                :div clojure.core//
                                :abs #(Math/abs (double %))
                                :sqrt #(Math/sqrt (double %))
                                :scalar? number?
                                :one 1.0
                                :zero 0.0
                                := ==
                                :supports-equality? true
                                :> >
                                :>= >=
                                :< <
                                :<= <=}))
(defn- round
  [^long precision ^double d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn fp [precision]
  (map->Specialisation {:add (comp (partial round precision) +)
                        :mul (comp (partial round precision) *)
                        :sub (comp (partial round precision) -)
                        :div (comp (partial round precision) clojure.core//)
                        :abs (comp (partial round precision) #(Math/abs (double %)))
                        :sqrt (comp (partial round  precision) #(Math/sqrt (double %)))
                        :scalar? number?
                        :one 1.0
                        :zero 0.0
                        := ==
                        :supports-equality? true
                        :> >
                        :>= >=
                        :< <
                        :<= <=}))

(deftest test-fixed-point
  (testing "quick fixed-point specialisation"
    (is (== 6.2832 (add (fp 4) Math/PI Math/PI)))))

(deftest test-addition
  (testing "matrix addition"
    (is (= [5.0] (add real [3.0] [2.0])))
    (is (= [[6.0]] (add real [[2.0]] [[4.0]])))
    (is (= [[[6.0]]] (add real [[[2.0]]] [[[4.0]]])))))

(deftest test-subtraction
  (testing "unary subtraction"
    (is (== (- 10) (sub real  10)))
    (is (cm/equals (sub real [1 2]) (op/- [1 2]))))
  (testing "matrix subtraction"
    (is (= [1.0] (sub real (cm/array [3.0]) [2.0])))
    (is (= [[8.0]] (sub real (cm/array [[12.0]]) [[4.0]])))
    (is (= [[[8.0]]] (sub real (cm/array [[[12.0]]]) [[[4.0]]]))))
  (testing "mutable sub"
    (let [v (cm/mutable-matrix [10 10])]
      (sub! real v [1 2] [1 2])
      (is (cm/equals [8 6] v))))
  (testing "arity 3 sub real regression"
    (is (cm/equals [-1 -2] (sub real [1 2] [1 2] [1 2])))))

(deftest test-vector-ops
  (testing "vector distance"
    (is (== 1.0 (distance real [0 0][0 1])))
    (is (== 1.0 (distance real [1 0][0 0])))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (mul real 3 2)))
    (is (== 6 (scale real 3 2)))
    (is (== 6 (gmp/generic-pre-scale 3 2 real))))
  (testing "matrix scaling"
    (is (cm/equals [6] (mul real (cm/array [3]) 2)))
    (is (cm/equals [6] (mul real 2 (cm/array [3]))))
    (is (cm/equals [[6]] (mul real 2 (cm/array [[3]]))))
    (is (cm/equals [[6]] (mul real (cm/array [[2]]) 3)))
    (is (cm/equals [[6]] (mul real (cm/array 2) (cm/array [[3]]))))
    (is (cm/equals [[6]] (mul real (cm/array [[2]]) (cm/array 3))))))

(deftest test-divide
  (is (== 2 (div real 4 2)))
  (is (op/== [2 1] (div real [4 2] 2)))
  (is (op/== [1 1.5] (div real [2 3] 2)))
  (is (cm/equals [2 1] (div real 4 [2 4])))
  (is (cm/equals [[1 2] [2 1]] (div real [[4 8] [4 4]] [[4 4] [2 4]]))))


(defspec generic-behaves-like-normal-when-using-real-spec
  100
  (prop/for-all [a gen/int
                 b gen/int]
                (= (cm/add a b) (add real a b))))