(ns pegthing.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. from set [1, 3, 6, 10, 15]"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns a row number the position belongs to:
    row 1: 1
    row 2: 2, 3
    etc..."
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos src-pos nei-pos dest-pos]
  (if (<= dest-pos max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] nei-pos))
            board
            [[src-pos dest-pos] [dest-pos src-pos]])
    board))

(defn connect-right
  "Given a board and a position, form connections toward E if possible"
  [board max-pos pos]
  (let [nei-pos (inc pos)
        dest-pos (inc nei-pos)]
    (if-not (or (triangular? nei-pos) (triangular? pos))
      (connect board max-pos pos nei-pos dest-pos)
      board)))

(defn connect-down-left
  "Given a board and position, form connections toward SW, if possible"
  [board max-pos pos]
  (let [row (row-num pos)
        nei-pos (+ row pos)
        dest-pos (+ 1 row nei-pos)]
    (connect board max-pos pos nei-pos dest-pos)))

(defn connect-down-right
  "Given a board and position, form connections toward SE, if possible"
  [board max-pos pos]
  (let [row (row-num pos)
        nei-pos (+ 1 row pos)
        dest-pos (+ 2 row nei-pos)]
    (connect board max-pos pos nei-pos dest-pos)))

(defn add-pos
  "reduces on a vector of function applying each in turn to build a board
    - Pegs the position and then adds connections toward E, SW, SE"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board fn-connection-creation]
              (fn-connection-creation new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Create a new peg board given number of rows
    - first create initial empty board and get max-pos
    - reduce range of 1 to max-pos+1 and reduce using add-pos"
  [row-count]
  (let [initial-board {:row-count row-count}
        max-pos (row-tri row-count)]
    (reduce (fn [acc-board pos] (add-pos acc-board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))
