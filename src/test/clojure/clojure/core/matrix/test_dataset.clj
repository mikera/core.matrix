(ns clojure.core.matrix.test-dataset
  (:require [clojure.core.matrix.impl.dataset :as ds]
            [clojure.core.matrix.compliance-tester :as compliance]
            [clojure.test :refer :all]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.utils :refer [error?]]
            [clojure.core.matrix.dataset :refer :all]))

(deftest test-construct-dataset
  (let [ds1 (dataset [:a :b] (matrix [[1 4] [2 5] [3 6]]))
        ds2 (dataset (matrix [[1 4] [2 5] [3 6]]))
        ds3 (dataset (sorted-map :a [1 2 3] :b [4 5 6]))
        ds4 (dataset [{:a 1 :b 2} {:a 4 :b 5}])
        ds5 (dataset [:c :a :b] [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}])
        ds6 (dataset [:vec :ndarray :double-array]
                     {:vec (matrix :persistent-vector [1 2 3])
                      :ndarray (matrix :ndarray [4 5 6])
                      :double-array (matrix :double-array [7 8 9])})]
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
    (is (vector? (first (columns ds6))))))

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
    (is (equals [[2 3] [4 5]] (emap inc ds)))
    (is (equals [[2 12] [4 14]] (emap + ds [1 10])))
    (is (equals [[111 112] [113 114]] (emap + ds 10 100)))
    (is (equals [[102 112] [104 114]] (emap + ds 100 [1 10])))))

(defn- round-trip [x]
  (read-string (pr-str x)))

(deftest test-round-trip 
  (let [ds (dataset [:a :b] [[1 2] ["Bob" "Mike"]])]
    (is (= ds (round-trip ds)))
    (let [dr (second (rows ds))]
      (is (= dr (round-trip dr))))))

(deftest instance-tests
  (let [dset (ds/dataset-from-columns [:bar :baz] [["Foo" "Bar"] [1 2]])]
    
    (compliance/instance-test dset)
    (compliance/instance-test (slice dset 0 1))))

(deftest compliance-test
  (compliance/compliance-test ds/CANONICAL-OBJECT))
