(ns pegthing.core
  (require [clojure.set :as set])
  (require [clojure.string :as cstr])
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

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at a given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at a given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "remove peg from p1 and place it at p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "return a map of all valid moves for pos, where the key is the destination
   and the value is the jumped position - taking into account wether destination
   is pegged"
  [board src-pos]
  (into {}
        (filter (fn [[dest-pos jumped-pos]]
                    (and (not (pegged? board dest-pos))
                         (pegged? board src-pos)
                         (pegged? board jumped-pos)))
                (get-in board [src-pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Truthy if any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board)) ;; first position that is truthy for valid-moves
        (map first (filter #(get (second %) :pegged) board)))) ;; sequence of all pegged positions

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

;; the book kind of skips over colorize so I copied it from
;; https://github.com/flyingmachine/pegthing/blob/master/src/pegthing/core.clj#L145-L159
(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))
;; end copy

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
          (colorize "0" :blue)
          (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0)) ;;start-incl: number at end of previous row or 0
         (inc (row-tri row-num))))            ;;end-excl: number at end of row

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:row-count board))
       (cstr/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:row-count board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (cstr/trim (read-line))]
     (if (empty? input)
       default
       (cstr/lower-case input)))))

(defn characters-as-strings
  "take a string and return a collection of letters with all non-alpha input discarded"
  [instr]
  (map str (filter #(<= alpha-start (int %) alpha-end)
                   (seq instr))))

(defn user-entered-invalid-move
  "Handles the next step after a suer has entered an INvalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handles the next step after a suer has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  "Read the player's input for a move and execute it"
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  "Announce that the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
  (prompt-rows))
