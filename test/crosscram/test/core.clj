(ns crosscram.test.core
  (:use [crosscram.core])
  (:use [clojure.test]))

(deftest basics
  (is (= (opposite :horizontal) :vertical))
  (is (= (opposite :vertical) :horizontal)))

(defn calvinist
  "Make a bot that will return exactly this sequence of moves."
  [& moves]
  (let [moves (vec moves)]
    (fn [game]
      (get moves (quot (count (:history game)) 2)))))

(deftest termination
  ;; Horizontal wins, some spaces remain
  ;; 00-1
  ;; -221
  (let [bot-h (calvinist [[0 0] [0 1]] [[1 1] [1 2]])
        bot-v (calvinist [[3 0] [3 1]])
        game (new-game 2 4)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 3))
    (is (= (winner done) :horizontal)))
  ;; Completely filled board
  ;; 0013
  ;; 2213
  (let [bot-h (calvinist [[0 0] [0 1]] [[1 0] [1 1]])
        bot-v (calvinist [[2 0] [2 1]] [[3 0] [3 1]])
        game (new-game 2 4)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 4))
    (is (= (winner done) :vertical)))
  ;; Vertical wins, vertical spaces remain
  ;; 00-
  ;; -1-
  ;; -1-
  (let [bot-h (calvinist [[0 0] [0 1]])
        bot-v (calvinist [[1 1] [1 2]])
        game (new-game 3 3)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 2))
    (is (= (winner done) :vertical)))
  ;; Horizontal wins, horizontal spaces remain
  ;; 22-
  ;; 001
  ;; --1
  (let [bot-h (calvinist [[1 0] [1 1]] [[0 0] [0 1]])
        bot-v (calvinist [[2 1] [2 2]])
        game (new-game 3 3)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 3))
    (is (= (winner done) :horizontal))))
