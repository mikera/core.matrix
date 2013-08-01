(ns clojure.core.matrix.docgen.bench
  (:require [criterium.core :as cr]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.implementations :as imp]
            [clojure.core.matrix.multimethods :as mm]
            [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.docgen.common :as c]))

(def conf {:tobench [:vectorz :ndarray]
           :reference :vectorz
           :fast-bench true})

(defn rand-mtx
  ([n] (rand-mtx n 1000 identity))
  ([n max caster]
     (letfn [(rand-row [] (->> #(rand-int max)
                               repeatedly (map caster) (take n) vec))]
       (->> rand-row repeatedly (take n) vec))))

(def tests
  (array-map
   :vectorz {:name "vectorz"
             :constructor #(m/array :vectorz %)
             :counts [5 50 500 1000]}
   :vecs {:name "persistent vectors"
          :constructor identity
          :counts [5 50]}
   :ndarray {:name "ndarray"
             :constructor #(m/array :ndarray %)
             :counts [5 50]}
   :ndarray-double {:name "ndarray-double"
                    :constructor #(m/array :ndarray-double %)
                    :counts [5 50 500 1000]}))

(defn mmultiply-bench []
  (doseq [[_ {:keys [name constructor counts]}] tests]
    (println (str name ":"))
    (doseq [n counts]
      (let [[ma mb] (->> #(rand-mtx n)
                         repeatedly (map constructor) (take 2))]
        (binding [cr/*final-gc-problem-threshold* 0.5]
          (->> (cr/quick-benchmark (mp/matrix-multiply ma mb) {})
               :mean
               (cr/report-point-estimate (str "n=" n ":"))))))))
