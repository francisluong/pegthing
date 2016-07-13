(ns pegthing.core-test
  (:require [clojure.test :refer :all]
            [pegthing.core :refer :all]))

(deftest tri-test
  (testing "taking from tri"
    (is (= [1 3 6 10 15] (take 5 tri)))
    (is (= [1 3 6 10] (take 4 tri)))))

(deftest triangular-test
  (testing "triangularity of known numbers"
    (is (triangular? 6))
    (is not (triangular? 5))))
