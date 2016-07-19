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
            4 {:connections {1 2}}} (connect {} 15 1 2 4)))
    (is (= {} (connect {} 15 15 16 17))))
  (testing "assoc-in/get-in deep map work"
    (def x-map {:cookie {:monster {:vocals "Finntroll"}}})
    (is (= x-map (assoc-in {} [:cookie :monster :vocals] "Finntroll")))
    (is (= {:vocals "Finntroll"} (get-in x-map [:cookie :monster])))
    ; # assoc-in in-hash key-list value
    (is (= {1 {:connections {4 2}}} (assoc-in {} [1 :connections 4] 2)))))

(deftest connect-right-test
  (testing "connect-right"
    (is (= {} (connect-right {} 15 3)))
    (is (= {} (connect-right {} 15 1)))
    (is (= {4 {:connections {6 5}}
            6 {:connections {4 5}}} (connect-right {} 15 4)))))

(deftest connect-down-right-test
  (testing "connect-down-right"
    (is (= {} (connect-down-right {} 15 15)))
    (is (= {1 {:connections {6 3}}
            6 {:connections {1 3}}} (connect-down-right {} 15 1)))
    (is (= {2 {:connections {9 5}}
            9 {:connections {2 5}}} (connect-down-right {} 15 2)))
    (is (= {3  {:connections {10 6}}
            10 {:connections {3 6}}} (connect-down-right {} 15 3)))))

(deftest connect-down-left-test
  (testing "connect-down-left"
    (is (= {} (connect-down-left {} 15 15)))
    (is (= {1 {:connections {4 2}}
            4 {:connections {1 2}}} (connect-down-left {} 15 1)))
    (is (= {2 {:connections {7 4}}
            7 {:connections {2 4}}} (connect-down-left {} 15 2)))
    (is (= {3 {:connections {8 5}}
            8 {:connections {3 5}}} (connect-down-left {} 15 3)))))

(deftest add-pos-test
  (testing "add-pos"
    (def x-board-add-pos {1 {:pegged true, :connections {4 2, 6 3}}
                          4 {:connections {1 2}}
                          6 {:connections {1 3}}})
    (is (= x-board-add-pos (add-pos {} 15 1)))))

(def x-board-full {:row-count 5
                    1 {:connections {4 2, 6 3}, :pegged true}
                    2 {:connections {7 4, 9 5}, :pegged true}
                    3 {:connections {8 5, 10 6}, :pegged true}
                    4 {:connections {1 2, 6 5, 11 7, 13 8}, :pegged true}
                    5 {:connections {12 8, 14 9}, :pegged true}
                    6 {:connections {1 3, 4 5, 13 9, 15 10}, :pegged true}
                    7 {:connections {2 4, 9 8}, :pegged true}
                    8 {:connections {3 5, 10 9}, :pegged true}
                    9 {:connections {2 5, 7 8}, :pegged true}
                    10 {:connections {3 6, 8 9}, :pegged true}
                    11 {:connections {4 7, 13 12}, :pegged true}
                    12 {:connections {5 8, 14 13}, :pegged true}
                    13 {:connections {4 8, 6 9, 11 12, 15 14}, :pegged true}
                    14 {:connections {5 9, 12 13}, :pegged true}
                    15 {:connections {6 10, 13 14}, :pegged true}})

(def x-board-empty-1 {:row-count 5
                      1 {:connections {4 2, 6 3}, :pegged false}
                      2 {:connections {7 4, 9 5}, :pegged true}
                      3 {:connections {8 5, 10 6}, :pegged true}
                      4 {:connections {1 2, 6 5, 11 7, 13 8}, :pegged true}
                      5 {:connections {12 8, 14 9}, :pegged true}
                      6 {:connections {1 3, 4 5, 13 9, 15 10}, :pegged true}
                      7 {:connections {2 4, 9 8}, :pegged true}
                      8 {:connections {3 5, 10 9}, :pegged true}
                      9 {:connections {2 5, 7 8}, :pegged true}
                      10 {:connections {3 6, 8 9}, :pegged true}
                      11 {:connections {4 7, 13 12}, :pegged true}
                      12 {:connections {5 8, 14 13}, :pegged true}
                      13 {:connections {4 8, 6 9, 11 12, 15 14}, :pegged true}
                      14 {:connections {5 9, 12 13}, :pegged true}
                      15 {:connections {6 10, 13 14}, :pegged true}})

(def x-board-empty-2 {:row-count 5
                      1 {:connections {4 2, 6 3}, :pegged true}
                      2 {:connections {7 4, 9 5}, :pegged false}
                      3 {:connections {8 5, 10 6}, :pegged true}
                      4 {:connections {1 2, 6 5, 11 7, 13 8}, :pegged true}
                      5 {:connections {12 8, 14 9}, :pegged true}
                      6 {:connections {1 3, 4 5, 13 9, 15 10}, :pegged true}
                      7 {:connections {2 4, 9 8}, :pegged true}
                      8 {:connections {3 5, 10 9}, :pegged true}
                      9 {:connections {2 5, 7 8}, :pegged true}
                      10 {:connections {3 6, 8 9}, :pegged true}
                      11 {:connections {4 7, 13 12}, :pegged true}
                      12 {:connections {5 8, 14 13}, :pegged true}
                      13 {:connections {4 8, 6 9, 11 12, 15 14}, :pegged true}
                      14 {:connections {5 9, 12 13}, :pegged true}
                      15 {:connections {6 10, 13 14}, :pegged true}})

(deftest new-board-test
  (testing "new board"
    (is (= x-board-full (new-board 5)))))

(deftest peg-ops-test
  (testing "pegged?"
    (is (= false (pegged? x-board-empty-1 1)))
    (is (= true (pegged? x-board-empty-1 2))))
  (testing "remove-peg"
    (is (= x-board-empty-1 (remove-peg x-board-full 1)))
    (is (= x-board-empty-1 (remove-peg x-board-empty-1 1))))
  (testing "place-peg"
    (is (= x-board-full (place-peg x-board-empty-1 1)))
    (is (= x-board-full (place-peg x-board-full 1))))
  (testing "move-peg"
    (is (= x-board-empty-2 (move-peg x-board-empty-1 2 1)))
    (is (= x-board-empty-2 (move-peg x-board-empty-2 2 1)))
    (is (= x-board-empty-2 (move-peg x-board-full 2 1)))
    (is (= x-board-empty-1 (move-peg x-board-full 1 2)))))
