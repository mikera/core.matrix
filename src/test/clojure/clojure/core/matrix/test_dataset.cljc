(ns clojure.core.matrix.test-dataset
  (:require [clojure.core.matrix.impl.dataset :as ds]
            [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.core.matrix :as cm :refer [columns label-index
                                                label labels
                                                matrix
                                                equals
                                                slice
                                                emap
                                                rows
                                                slices
                                                get-row
                                                e= e==]]
            [clojure.core.matrix.stats :as stats]
            #?@(:clj [[clojure.test :refer :all]
                      [clojure.core.matrix.macros :refer [error]]
                      [clojure.core.matrix.macros-clj :refer [error?]]
                      [clojure.core.matrix :refer [with-implementation]]
                      ]
                :cljs [[cljs.test :refer [do-report update-current-env!]]
                       [thinktopic.aljabr.core]
                       [cljs.reader :refer [read-string]]
                       ])
            [clojure.core.matrix.dataset :as cd :refer [dataset dataset?
                                                        column-names column-name
                                                        to-map
                                                        merge-datasets
                                                        rename-columns
                                                        join-rows
                                                        row-maps
                                                        column
                                                        column-index
                                                        select-rows
                                                        select-columns
                                                        replace-column join-columns
                                                        remove-columns
                                                        emap-column
                                                        emap-columns
                                                        dataset-row?
                                                        ]])
  #?(:cljs (:require-macros [clojure.core.matrix :refer [with-implementation]]
                            [clojure.core.matrix.macros :refer [error]]
                            [clojure.core.matrix.macros-cljs :refer [error?]]
                            [cljs.test :refer [deftest testing is]])))

(deftest test-construct-dataset
  (testing "dataset construction"
    (let [ds1 (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))
          ds2 (dataset (matrix [[1 4] [2 5] [3 6]]))
          ds3 (dataset (sorted-map :a [1 2 3] :b [4 5 6]))
          ds4 (dataset [{:a 1 :b 2} {:a 4 :b 5}])
          ds5 (dataset [:c :a :b] [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}])
          ds6 (dataset [:vec :ndarray :double-array]
                       {:vec (matrix :persistent-vector [1 2 3])
                        :ndarray (matrix :ndarray [4 5 6])
                        :double-array (matrix :double-array [7 8 9])})
          ds7 (dataset (seq ds5))]
      (is (= (column-names ds1) [:a :b]))
      (is (= (columns ds1) [[1 2 3] [4 5 6]]))
      (is (= (column-names ds2) [0 1]))
      (is (= (columns ds2) [[1 2 3] [4 5 6]]))
      (is (= (column-names ds3) [:a :b]))
      (is (= (columns ds3) [[1 2 3] [4 5 6]]))
      (is (= (into #{} (column-names ds4)) #{:a :b}))
      (is (= (into #{} (columns ds4)) #{[1 4] [2 5]}))
      (is (= (column-names ds5) [:c :a :b]))
      (is (= (columns ds5) [[3 6] [1 4] [2 5]]))
      (is (vector? (first (columns ds6))))
      (is (= (column-names ds5) (column-names ds7)))
      (is (= (columns ds5) (columns ds7))))))



(deftest test-regressions
  (testing "Should be possible to create a dataset with element types that the current implementation does not support"
    (with-implementation :vectorz
      (dataset [:a :b] '(["A" 2] ["B" 3])))))

(deftest test-column-name
  (let [ds (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))]
    (is (= (column-name ds 0) :a))
    (is (= (column-name ds 1) :b))
    (is (= (column-name ds 0) (label ds 1 0)))
    (is (= (column-name ds 1) (label ds 1 1)))
    (is (= (column-names ds) (labels ds 1)))
    (is (= (column-names ds) (column-names (get-row ds 1))))
    (is (error? (column-name ds 2)))
    (is (= 0 (column-index ds :a) (label-index ds 1 :a)))
    (is (= 1 (column-index ds :b) (label-index ds 1 :b)))
    (is (nil? (column-index ds :c)))
    (is (nil? (label-index ds 1 :c)))
    (is (nil? (label-index ds 0 :a)))))

(deftest test-select-columns
  (let [ds1 (dataset [:a :b :c] (matrix [[1 4 9] [2 5 9] [3 6 9] [4 7 9]]))
        ds2 (dataset [:a :b :c] [])]
    (is (= (select-columns ds1 [:a :b])
           (dataset [:a :b] (matrix [[1 4] [2 5] [3 6] [4 7]]))))
    (is (= (select-columns ds2 [:a :b]) (dataset [:a :b] [])))
    (is (= (remove-columns ds1 [:a :b])
           (dataset [:c] (matrix [[9] [9] [9] [9]]))))))

(deftest test-select-rows
  (let [ds (dataset [:a :b :c] (matrix [[1 4 9] [2 5 9] [3 6 9] [4 7 9]]))]
    (is (= (select-rows ds [1 2])
           (dataset [:a :b :c] (matrix [[2 5 9] [3 6 9]]))))))

(deftest test-row-emap
  (is (equals [2 3] (emap inc (first (slices (dataset [[1 2] [-1 -2]])))))))

(deftest test-to-map
  (let [ds (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))]
    (is (= (to-map ds) {:a [1 2 3] :b [4 5 6]}))))

(deftest test-merge-datasets
  (let [ds1 (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))
        ds2 (dataset [:b :c] (matrix [[8 1] [8 9] [8 6]]))]
    (is (equals (merge-datasets ds1 ds2)
                (dataset [:a :b :c]
                         (matrix [[1 8 1] [2 8 9] [3 8 6]]))))))

(deftest test-rename-columns
  (let [ds (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))]
    (is (= (rename-columns ds {:a :c
                               :b :d})
           (dataset [:c :d] (matrix [[1 4] [2 5] [3 6]]))))))

(deftest test-replace-column
  (let [ds (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))]
    (is (= (replace-column ds :a [9 9 9])
           (dataset [:a :b] (matrix [[9 4] [9 5] [9 6]]))))
    (is (= (emap-column ds :b * 3)
           (dataset [:a :b] (matrix [[1 12] [2 15] [3 18]]))))))

(deftest test-join-rows
  (let [ds1 (dataset [:a :b] [[1 4] [2 5] [3 6]])
        ds2 (dataset [:b :a] [[8 5] [8 6] [8 7]])]
    (is (= (join-rows ds1 ds2)
           (dataset [:a :b] [[1 4] [2 5] [3 6] [5 8] [6 8] [7 8]])))))

(deftest test-join-columns
  (let [ds1 (dataset [:a :b] [[1 4] [2 5] [3 6]])
        ds2 (dataset [:c :d] [[8 5] [8 6] [8 7]])]
    (is (= (join-columns ds1 ds2)
           (dataset [:a :b :c :d] [[1 4 8 5] [2 5 8 6] [3 6 8 7]])))))

(deftest test-row-maps
  (let [ds (dataset [:a :b] [[1 4] [2 5] [3 6]])]
    (is (= (row-maps ds) [{:a 1 :b 4} {:a 2 :b 5} {:a 3 :b 6}]))))

(deftest test-emap
  (let [ds (dataset [:a :b] [[1 2] [3 4]])]
    (equals [[2 3] [4 5]] (emap inc ds))
    (equals [[2 12] [4 14]] (emap + ds [1 10]))
    (equals  [[111 112] [113 114]] (emap + ds 10 100))
    (equals [[102 112] [104 114]] (emap + ds 100 [1 10]))))

(deftest test-emap-columns
  (let [kidneys (dataset ["State" "Charge"] [["FL" "0.4"] ["FL" "0.6"] ["NY" "0.8"]])
        dparse  #( #?(:clj Double/parseDouble :cljs js/parseFloat) %)]
    (testing "Using column map"
      (let [kidneys (emap-columns kidneys {"Charge" dparse} )
            groups (group-by first (slices kidneys))]
        (is (= {"FL" 0.5, "NY" 0.8}
               (into {} (for [[state rows] groups] [state (stats/mean (mapv second rows))]))))))
    (testing "Single arity"
      (let [kidneys (emap-columns kidneys ["Charge"] dparse)
            groups (group-by first (slices kidneys))]
        (is (= {"FL" 1.0, "NY" 0.8}
               (into {} (for [[state rows] groups] [state (stats/sum (mapv second rows))]))))))
    (testing "Variable arity"
      (let [kidneys (emap-columns kidneys ["Charge"] (fn [charge _] (dparse charge)) :foo)
            groups (group-by first (slices kidneys))]
        (is (= {"FL" 1.0, "NY" 0.8}
               (into {} (for [[state rows] groups] [state (stats/sum (mapv second rows))]))))))))

(deftest test-datasetrow
  (let [col-names [:a :b]
        ds (dataset col-names [[1 "Bob"] [2 "Mike"]])
        dr (clojure.core.matrix.impl.dataset/wrap-row ds 1)]
    (testing "Column names"
      (is (= col-names (column-names dr))))
    (testing "Column access"
      (is (= "Mike" (column dr :b)))
      (is (error? (column dr :c))))
    (testing "Map-like access"
      (is (= {:a 2 :b "Mike"} (to-map dr))))
    (testing "DataSetRow as vector implementation"
      (is (= [2 "Mike"] (vec dr)))
      (is (= "Mike" (nth dr 1)))
      (is (= :not-found (nth dr 2 :not-found)))
      (is (= :not-found (nth dr -1 :not-found))))
    (testing "type tests"
      (is (dataset-row? (first (seq (dataset [:a :b] [[1 2] [3 4]]))))))))

(deftest seq-filtering
  (let [d1 (dataset [:a :b] [[1 2] [3 4]])]
    (testing "filtering of dataset" 
      (is (= (dataset [:a :b] [[3 4]]) (dataset (filter #(= 3 (cd/column % :a)) d1))))
      (is (= (dataset [:a :b] [[3 4]]) (dataset (remove #(= 1 (cd/column % :a)) d1)))))))

(defn- round-trip [x]
  (read-string (pr-str x)))

#?(:clj
   (do
     (deftest test-round-trip
       (let [ds (dataset [:a :b] [[1 2] ["Bob" "Mike"]])]
         (is (= ds (round-trip ds)))
         (let [dr (second (rows ds))]
           (is (= dr (round-trip dr))))))    
     (deftest compliance-test
       (compliance/compliance-test ds/CANONICAL-OBJECT))
     (deftest instance-tests
       (let [dset (ds/dataset-from-columns [:bar :baz] [["Foo" "Bar"] [1 2]])]
         (compliance/instance-test dset)
         (compliance/instance-test (second (slices dset)))
         (compliance/instance-test (slice dset 1 0))))))

