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

(defn mmultiply-bench []
  (let [n 70]
    (println "Test matrix size:" n)
    (println "Vectorz:")
    (let [[ma mb] (->> #(rand-mtx n)
                       repeatedly (map #(m/array :vectorz %)) (take 2))]
      (cr/report-result (cr/quick-benchmark (mp/matrix-multiply ma mb) {})))
    (println "-----")
    (println "Persistent vectors:")
    (let [[ma mb] (->> #(rand-mtx n)
                       repeatedly (take 2))]
      (cr/report-result (cr/quick-benchmark (mp/matrix-multiply ma mb) {})))
    (println "-----")
    (println "NDArray:")
    (let [[ma mb] (->> #(rand-mtx n)
                       repeatedly (map #(m/array :ndarray %)) (take 2))]
      (cr/report-result (cr/quick-benchmark (mp/matrix-multiply ma mb) {})))
    (println "-----")
    (println "NDArrayDouble:")
    (let [[ma mb] (->> #(rand-mtx n)
                       repeatedly (map #(m/array :ndarray-double %)) (take 2))]
      (cr/report-result (cr/quick-benchmark (mp/matrix-multiply ma mb) {})))))
