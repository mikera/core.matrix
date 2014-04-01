(ns clojure.core.matrix.test-generic
  (:use clojure.test)
  (:use [clojure.core.matrix.generators])
  (:require [clojure.core.matrix :as cm])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.generic :refer :all])
  (:require [clojure.core.matrix.generic-protocols :as gmp])
  (:require [clojure.test.check :as sc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.properties :as prop])
  (:require [clojure.test.check.clojure-test :as ct :refer (defspec)])
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
(defn- fp-round
  [^long precision ^double d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))


(defn fp [precision]
  (map->Specialisation {:add (comp (partial fp-round precision) +)
                        :mul (comp (partial fp-round precision) *)
                        :sub (comp (partial fp-round precision) -)
                        :div (comp (partial fp-round precision) clojure.core//)
                        :abs (comp (partial fp-round precision) #(Math/abs (double %)))
                        :sqrt (comp (partial fp-round  precision) #(Math/sqrt (double %)))
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

;;TODO object array gives bad results here
(defspec test-gen-elem-wise-operations 100
  (prop/for-all [[a b] (gen-conforming-arrays
                        2 :implementations [:ndarray :persistent-vector])]
                (is (cm/equals (cm/add a b) (add real a b)))
                (is (cm/equals (cm/sub a b) (sub real a b)))
                (is (cm/equals (cm/mul a b) (mul real a b)))
                (is (cm/equals (cm/emul a b) (emul real a b)))
                (is (cm/equals (cm/e* a b) (e* real a b)))
                (when-not (some (partial == 0.0) (cm/eseq b))
                  (is (cm/equals (cm/div a b) (div real a b))))
                (is (cm/equals (cm/negate a) (negate real a)))))


(defspec test-gen-vector-operations 100
  (prop/for-all [a (gen-array :implementations [:ndarray :persistent-vector]
                              :max-dim 1 :min-dim 1)]
                (is (cm/equals (cm/length a) (length real a)))
                (is (cm/equals (cm/length-squared a) (length-squared
                                                      real a)))))

(defspec test-gen-mmul 20
  (prop/for-all [[a b] (gen/such-that
                        (fn [[l r]] (= (last (cm/shape l))
                                       (first (cm/shape r))))
                        (gen/tuple (gen-matrix :implementations [:ndarray :persistent-vector]) (gen-matrix :implementations [:ndarray :persistent-vector])))]
                (is (cm/equals (cm/mmul a b) (mmul real a b)))))

