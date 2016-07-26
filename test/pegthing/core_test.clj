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


(defn board-missing
  [empty-positions]
  (reduce (fn [board rem-pos] (remove-peg board rem-pos))
          (new-board 5)
          empty-positions))

(def board-4-rem (board-missing [4]))

(deftest valid-moves-test
  (testing "with 4 removed"
    (is (not (pegged? board-4-rem 4)))
    (is (= {4 2} (valid-moves board-4-rem 1)))
    (is (= {4 5} (valid-moves board-4-rem 6)))
    (is (= {4 7} (valid-moves board-4-rem 11)))
    (is (= {4 8} (valid-moves board-4-rem 13)))
    (is (= {} (valid-moves board-4-rem 5)))
    (is (= {} (valid-moves board-4-rem 8)))
    (is (= {} (valid-moves board-4-rem 4)))
    (is (= {} (valid-moves (remove-peg board-4-rem 1) 4)))))

(deftest valid-move?-test
  (testing "valid-move?"
    (is (not (pegged? board-4-rem 4)))
    (is (= 2 (valid-move? board-4-rem 1 4)))
    (is (= nil (valid-move? board-4-rem 8 4)))))

(deftest make-move-test
  (testing "make-move"
    (is (= (board-missing [1 2]) (make-move board-4-rem 1 4)))
    (is (= (board-missing [1 4 7]) (make-move (board-missing [1 2]) 7 2))))
  (testing "make-move invalid move"
    (is (= nil (make-move (board-missing [1 2]) 1 4)))))

(deftest can-move-test
  (testing "can-move"
    (is (can-move? (board-missing [1])))
    (is (can-move? (board-missing [1 2 3 4 5 6 7 8 9 11 12 13 14]))))
  (testing "cant-move"
    (is (not (can-move? (new-board 5))))
    (is (not (can-move? (board-missing [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])))))
  (testing "internals"
    (testing "tuples from filter"
      (is (=  [[1 {:connections {4 2, 6 3}, :pegged true}]
               [2 {:connections {7 4, 9 5}, :pegged false}]]
              (filter
                  (fn [x] x)
                  {1 {:connections {4 2, 6 3}, :pegged true}
                   2 {:connections {7 4, 9 5}, :pegged false}})))
      (is (= [1 3 5] (filter odd? (range 6)))))
    (testing "first and second and get"
      (is (= 1 (first [1 {:connections {4 2, 6 3}, :pegged true}])))
      (is (= {:connections {4 2, 6 3}, :pegged true}
             (second [1 {:connections {4 2, 6 3}, :pegged true}])))
      (is (= true (get {:connections {4 2, 6 3}, :pegged true} :pegged))))
    (testing "partial"
      (let [add-one (partial + 1)]
        (is (= 2 (add-one 1)))))))

(deftest letters-test
  (testing "letters"
    (def x-letters ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])
    (is (= x-letters letters))))

(def empty-colored (colorize "-" :red))
(def pegged-colored (colorize "0" :blue))

(deftest render-pos-test
  (testing "render-pos-internals"
    (is (= \b (nth "abc" 1)))
    (is (= "c" (nth ["a" "b" "c"] 2))))
  (testing "colorize"
    (is (not (= "0" pegged-colored))))
  (testing "render-pos"
    (is (= (str "a" empty-colored) (render-pos (board-missing [1]) 1)))
    (is (= (str "b" pegged-colored) (render-pos (board-missing [1]) 2)))
    (is (= (str "a" pegged-colored) (render-pos (board-missing [2]) 1)))))

(deftest row-positions-test
  (testing "row-positions"
    (is (= [1] (row-positions 1)))
    (is (= [2 3] (row-positions 2)))
    (is (= [4 5 6] (row-positions 3)))
    (is (= [7 8 9 10] (row-positions 4)))
    (is (= [11 12 13 14 15] (row-positions 5))))
  (testing "row-positions-internals"
    (is (= 0 (dec 1)))
    (is (= nil (row-tri 0)))
    (is (= 1 (row-tri 1)))
    (is (= 0 (or nil 0)))
    (is (= 4 (dec 5)))
    (is (= 10 (row-tri 4)))
    (is (= 10 (or (row-tri (dec 5)) 0)))))

(deftest row-padding-test
  (testing "row-padding"
    (is (= "      " (row-padding 1 5)))
    (is (= "     " (row-padding 2 5)))
    (is (= "   " (row-padding 3 5)))
    (is (= "  " (row-padding 4 5)))
    (is (= "" (row-padding 5 5)))))

(deftest render-row-test
  (testing "render row"
    (is (= (str "      a" empty-colored) (render-row (board-missing [1]) 1)))
    (is (= (str "     b" pegged-colored " c" pegged-colored)
           (render-row (board-missing [1]) 2))))
  (testing "render-row internals"
    (is (= 5 (:row-count (board-missing [1]))))))

(deftest print-board-test
  (testing "print-board"
    (is (= nil (print-board (board-missing [1]))))))

(deftest letter-pos-test
  (testing "letter->pos"
    (is (= 1 (letter->pos "a")))
    (is (= 2 (letter->pos "b")))
    (is (= 15 (letter->pos "o")))))

(deftest characters-as-strings-test
  (testing "characters-as-strings"
    (is (= ["a" "b"] (characters-as-strings "a  b")))))
