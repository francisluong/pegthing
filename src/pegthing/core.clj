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
