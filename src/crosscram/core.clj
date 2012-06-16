(ns crosscram.core
  "Functions for managing a crosscram game. This namespace has been deprecated;
crosscram.engine replaces it."
  (:require [crosscram.board :as board]))

(defn opposite [player]
  {:pre [(#{:horizontal :vertical} player)]
   :post [(keyword? %)]}
  (cond
    (= :horizontal player) :vertical
    :else :horizontal))

(defn over? [game]
  (not (board/can-play-horizontal? (:board game))))

(defn winner [game]
  {:post [(keyword? %)]}
  (:player (last (:history game))))

(defn play-piece
  "Try to play a horizontal piece. Game is assumed not over."
  [game pos-a pos-b]
  (cond

    ; do the two points form a valid piece?
    (not (board/horizontal-pair? pos-a pos-b))
    (throw (Exception. (format "Not a valid %s shape: %s"
                               (:next-player game) [pos-a pos-b])))

    ; is someone trying to play on a spot that is already
    ; occupied?
    (not (board/location-empty? (:board game) pos-a pos-b))
    (throw (Exception. "Can't move here, it's occupied"))

    ; ok, play the piece!
    :else
    (-> game
      (assoc :board
        (board/transpose
         (board/add-piece (:board game) (inc (count (:history game)))
                          pos-a pos-b)))
      (update-in [:history]
                 #(conj % {:player (:next-player game) :move [pos-a pos-b]}))
      (assoc :next-player
        (opposite (:next-player game)))
      (assoc :rows (:columns game))
      (assoc :columns (:rows game)))))

(defn new-game [rows columns]
  {:board (board/board rows columns)
   :rows rows
   :columns columns
   :next-player :horizontal
   :history []})

(defn play [game bot-a bot-b]
  "Play a game and return the resulting game-state."
  (loop [g game
         bot-funs (cycle [bot-a bot-b])]
    (if (over? g)
      g
      (let [new-game (apply play-piece g ((first bot-funs) g))]
        (recur new-game (rest bot-funs))))))

(defn score [game1 game2]
  {:pre [(keyword? game1), (keyword? game2)]}
  (let [pair [game1 game2]]
    (cond
      (= pair [:horizontal :vertical]) {:bot-a 1 :bot-b 0 :draws 0}
      (= pair [:vertical :horizontal]) {:bot-a 0 :bot-b 1 :draws 0}
      :else                            {:bot-a 0 :bot-b 0 :draws 1})))

(defn play-symmetric [game bot-a bot-b games-to-play]
  (loop [scoreboard {}]
    (if (= games-to-play (apply + (vals scoreboard)))
      scoreboard
      (let [g1 (winner (play game bot-a bot-b))
            g2 (winner (play game bot-b bot-a))]
        (recur (merge-with + scoreboard (score g1 g2)))))))
