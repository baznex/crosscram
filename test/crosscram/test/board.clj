(ns crosscram.test.board
  (:use clojure.test
        crosscram.board))

(deftest playable
  (are [c b] (= (boolean (@#'crosscram.board/two-nil? c)) b)
       [1 2 nil nil nil 3 4] true
       [nil nil] true
       [nil] false
       [] false
       [1 2 3] false
       [nil 1 2 nil] false
       [nil nil 3] true
       [5 6 7 nil nil] true)
  (is (can-play-horizontal? (board 13 13))))
