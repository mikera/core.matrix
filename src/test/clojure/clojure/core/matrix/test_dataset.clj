(ns clojure.core.matrix.test-dataset
  (:use clojure.test)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.dataset)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.dataset :as ds])
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.compliance-tester]))

(deftest test-construct-dataset
  (let [ds1 (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))
        ds2 (dataset (matrix [[1 2 3] [4 5 6]]))
        ds3 (dataset (sorted-map :a [1 2 3] :b [4 5 6]))
        ds4 (dataset [{:a 1 :b 2} {:a 4 :b 5}])]
    (is (= (column-names ds1) [:a :b]))
    (is (= (columns ds1) [[1 2 3] [4 5 6]]))
    (is (= (column-names ds2) [0 1 2]))
    (is (= (columns ds2) [[1 4] [2 5] [3 6]]))
    (is (= (column-names ds3) [:a :b]))
    (is (= (columns ds3) [[1 2 3] [4 5 6]]))
    (is (= (into #{} (column-names ds4)) #{:a :b}))
    (is (= (into #{} (columns ds4)) #{[1 4] [2 5]}))))

(deftest test-column-name
  (let [ds (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))]
    (is (= (column-name ds 0) :a))
    (is (= (column-name ds 1) :b))))

(deftest test-select-column
  (let [ds (dataset [:a :b :c] (matrix [[1 2 3 4]
                                        [4 5 6 7]
                                        [9 9 9 9]]))]
    (is (= (select-columns ds [:a :b])
           (dataset [:a :b] (matrix [[1 2 3 4]
                                     [4 5 6 7]]))))
    (is (= (except-columns ds [:a :b])
           (dataset [:c] (matrix [[9 9 9 9]]))))))

(deftest test-select-column
  (let [ds (dataset [:a :b :c] (matrix [[1 2 3 4]
                                        [4 5 6 7]
                                        [9 9 9 9]]))]
    (is (= (select-rows ds 0)
           (dataset [:a :b :c] (matrix [[1] [4] [9]]))))
    (is (= (select-rows ds [1 2])
           (dataset [:a :b :c] (matrix [[2 3] [5 6] [9 9]]))))))

(deftest test-to-map
  (let [ds (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))]
    (is (= (to-map ds) {:a [1 2 3] :b [4 5 6]}))))

(deftest test-merge-datasets
  (let [ds1 (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))
        ds2 (dataset [:b :c] (matrix [[8 8 8] [1 9 6]]))]
    (is (equals (merge-datasets ds1 ds2)
                (dataset [:a :b :c]
                         (matrix [[1 2 3]
                                  [8 8 8]
                                  [1 9 6]]))))))

(deftest test-rename-columns
  (let [ds (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))]
    (is (= (rename-columns ds {:a :c
                               :b :d})
           (dataset [:c :d] (matrix [[1 2 3] [4 5 6]]))))))

(deftest test-replace-column
  (let [ds (dataset [:a :b] (matrix [[1 2 3] [4 5 6]]))]
    (is (= (replace-column ds :a [9 9 9])
           (dataset [:a :b] (matrix [[9 9 9] [4 5 6]]))))
    (is (= (update-column ds :b * 3)
           (dataset [:a :b] (matrix [[1 2 3] [12 15 18]]))))))

(deftest test-conj-rows
  (let [ds1 (dataset [:a :b] [[1 2 3] [4 5 6]])
        ds2 (dataset [:b :a] [[8 8 8] [5 6 7]])]
    (is (= (conj-rows ds1 ds2)
           (dataset [:a :b] [[1 2 3 5 6 7]
                             [4 5 6 8 8 8]])))))

(deftest test-row-maps
  (let [ds (dataset [:a :b] [[1 2 3] [4 5 6]])]
    (is (= (row-maps ds) [{:a 1 :b 4} {:a 2 :b 5} {:a 3 :b 6}]))))

(deftest instance-tests
  (clojure.core.matrix.compliance-tester/instance-test
   (ds/construct-dataset [:bar] [["Foo"]])))

(deftest compliance-test
  (clojure.core.matrix.compliance-tester/compliance-test ds/CANONICAL-OBJECT))
