(ns crosscram.test.engine
  (:use [clojure.test]
        [crosscram.engine])
  (:require [crosscram.game :as game]))

(deftest test-over
  (let [game-not-over (-> (game/make-game [2 3] 0)
                          (game/move [[1 0] [1 1]]))
        game-over (-> game-not-over
                      (game/rotate-game)
                      (game/move [[2 0] [2 1]])
                      (game/rotate-game)
                      (game/move [[0 0] [0 1]]))]
    (is (not (over? (:board game-not-over))))
    (is (over? (:board game-over)))))

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
        game (game/make-game [2 4] 0)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 3))
    (is (= (winner done) 0)))
  ;; Completely filled board
  ;; 0013
  ;; 2213
  (let [bot-h (calvinist [[0 0] [0 1]] [[1 0] [1 1]])
        bot-v (calvinist [[2 0] [2 1]] [[3 0] [3 1]])
        game (game/make-game [2 4] 0)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 4))
    (is (= (winner done) 1)))
  ;; Vertical wins, vertical spaces remain
  ;; 00-
  ;; -1-
  ;; -1-
  (let [bot-h (calvinist [[0 0] [0 1]])
        bot-v (calvinist [[1 1] [1 2]])
        game (game/make-game [3 3] 0)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 2))
    (is (= (winner done) 1)))
  ;; Horizontal wins, horizontal spaces remain
  ;; 22-
  ;; 001
  ;; --1
  (let [bot-h (calvinist [[1 0] [1 1]] [[0 0] [0 1]])
        bot-v (calvinist [[2 1] [2 2]])
        game (game/make-game [3 3] 0)
        done (play game bot-h bot-v)]
    (is (= (count (:history done)) 3))
    (is (= (winner done) 0))))
