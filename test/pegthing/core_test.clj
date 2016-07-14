(ns pegthing.core-test
  (:require [clojure.test :refer :all]
            [pegthing.core :refer :all]))

(def the-triangular-five [1 3 6 10 15])
(deftest tri-test
  (testing "taking from tri"
    (is (= the-triangular-five (take 5 tri)))
    (is (= [1 3 6 10] (take 4 tri)))))

(deftest triangular-test
  (testing "triangularity of known numbers"
    (is (every? triangular? the-triangular-five))
    (is not (triangular? 5))))

(deftest row-tri-test
  (testing "row-tri values"
    (is (= 1 (row-tri 1)))
    (is (= 3 (row-tri 2)))
    (is (= 6 (row-tri 3)))
    (is (= 10 (row-tri 4)))
    (is (= 15 (row-tri 5)))))

(deftest row-num-test
  (testing "row-num for selected values"
    (is (= 1 (row-num 1)))
    (is (= 2 (row-num 2)))
    (is (= 3 (row-num 5)))
    (is (= 5 (row-num 14)))))

(deftest connect-test
  (testing "establishment of board position connections"
    (is (= {1 {:connections {4 2}}
            4 {:connections {1 2}}} (connect {} 15 1 2 4))))
  (testing "assoc-in/get-in deep map work"
    (def x-map {:cookie {:monster {:vocals "Finntroll"}}})
    (is (= x-map (assoc-in {} [:cookie :monster :vocals] "Finntroll")))
    (is (= {:vocals "Finntroll"} (get-in x-map [:cookie :monster])))
    (is (= {1 {:connections {4 2}}} (assoc-in {} [1 :connections 4] 2)))))
