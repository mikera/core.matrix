(deftest test-swap
  (testing "vector row swap"
    (is (== [0 2] (swap [2 0] 0 1)))
    (is (== [0 2] (swap [2 0] 1 0)))
    (is (== [0 2] (swap [[2 0]] 0 1))))
  (testing "matrix row swap"
    (is (== [[0 2] [2 0]] (swap [[2 0] [0 2]] 0 1)))
    (is (== [[0 2] [2 0] [1 1]] (swap [[1 1] [2 0] [0 2]] 0 2)))))

(deftest test-mul)

