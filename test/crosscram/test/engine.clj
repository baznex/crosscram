(ns crosscram.test.engine
  (:use [clojure.test]
        [clojure.template :only (do-template)]
        [crosscram.engine])
  (:require [crosscram.game :as game]))

(defn calvinist
  "Make a bot that will return exactly this sequence of moves."
  [& moves]
  (let [moves (vec moves)]
    (fn [game]
      (get moves (quot (count (:history game)) 2)))))

(deftest stepping
  (do-template
   [domino event*] (let [move (call-bot (game/make-game [2 4] 0)
                                        (calvinist domino))]
                     (is (number? (:duration move)))
                     (is (= (:player-id move) 0))
                     (is (= (dissoc move :duration :player-id) event*)))
   [[0 0] [0 1]] {:type :move, :move [[0 0] [0 1]]}
   [[5 0] [5 1]] {:type :invalid-move, :move [[5 0] [5 1]]}
   'xyz          {:type :player-error, :return 'xyz})
  (let [player-threw (call-bot (game/make-game [2 4] 0)
                               (fn [&_] (throw (ArithmeticException.))))]
    (is (number? (:duration player-threw)))
    (is (= (:player-id player-threw) 0))
    (is (= (:type player-threw) :player-error))
    (is (= (set (keys player-threw)) #{:type :player-id :duration :error}))
    (is (= (class (:error player-threw)) ArithmeticException)))
  ;; {:type :cant-move} is missing because call-bot is not invoked for that
  )

(deftest termination
  ;; Horizontal wins, some spaces remain
  ;; 00-1
  ;; -221
  (let [bot-h (calvinist [[0 0] [0 1]] [[1 1] [1 2]])
        bot-v (calvinist [[3 0] [3 1]])
        game (game/make-game [2 4] 0)
        done (play game [bot-h bot-v])]
    (is (= (count (:history done)) 4))
    (is (= (-> done :history peek) {:type :cant-move, :player-id 1}))
    (is (= (game/winner done) 0)))
  ;; Completely filled board
  ;; 0013
  ;; 2213
  (let [bot-h (calvinist [[0 0] [0 1]] [[1 0] [1 1]])
        bot-v (calvinist [[2 0] [2 1]] [[3 0] [3 1]])
        game (game/make-game [2 4] 0)
        done (play game [bot-h bot-v])]
    (is (= (count (:history done)) 5))
    (is (= (game/winner done) 1)))
  ;; Vertical wins, vertical spaces remain
  ;; 00-
  ;; -1-
  ;; -1-
  (let [bot-h (calvinist [[0 0] [0 1]])
        bot-v (calvinist [[1 1] [1 2]])
        game (game/make-game [3 3] 0)
        done (play game [bot-h bot-v])]
    (is (= (count (:history done)) 3))
    (is (= (game/winner done) 1)))
  ;; Horizontal wins, horizontal spaces remain
  ;; 22-
  ;; 001
  ;; --1
  (let [bot-h (calvinist [[1 0] [1 1]] [[0 0] [0 1]])
        bot-v (calvinist [[2 1] [2 2]])
        game (game/make-game [3 3] 0)
        done (play game [bot-h bot-v])]
    (is (= (count (:history done)) 4))
    (is (= (game/winner done) 0))))
