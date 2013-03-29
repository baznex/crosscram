(ns crosscram.engine
  "Game engine.

A bot is a function of [game -> domino].

The returned domino must be horizontal; that is, the column coordinates differ
by 1 but the row coordinates are equal. That is, every player plays
horizontal, and sees the other player as playing vertical. (The game engine
gives the second player a transposed view of the board.)"

  (:require [crosscram.game :as game]))

;; TODO(timmc:2012-05-23) Decide on tournament rules for bots throwing
;; exceptions, returning nil, returning bad dominoes...

;; TODO(timmc:2012-05-24) Wait, how would we even decide which player won a
;; 3-player game? Last player to place a tile before someone fails, or last
;; player standing after successive elimination?

;; TODO: Strip metadata from returned dominoes. Player could be storing state
;; there or otherwise be up to no good.

(def over?
  "Returns true if there are no horizontal moves possible, false otherwise.
Takes a board as argument."
  (complement game/can-move?))

(defn score [game1 game2]
  (let [pair [game1 game2]]
    (cond
      (= pair [0 1]) {:bot-a 1 :bot-b 0 :draws 0}
      (= pair [1 0]) {:bot-a 0 :bot-b 1 :draws 0}
      :else          {:bot-a 0 :bot-b 0 :draws 1})))

(defn winner [game]
  (mod (dec (count (:history game))) 2))

(defn play [game bot-a bot-b]
  "Play a game and return the resulting game-state."
  (loop [g game
         bot-funs (cycle [bot-a bot-b])]
    (if (over? (:board g))
      g
      (let [new-game (game/rotate-game (game/move g ((first bot-funs) g)))]
        (recur new-game (rest bot-funs))))))

(defn play-symmetric [game bot-a bot-b games-to-play]
  (let [play-round (fn [_]
                     (->> [(play game bot-a bot-b) (play game bot-b bot-a)]
                       (map winner)
                       (apply score)))]
    (->> (pmap play-round (range games-to-play))
      (reduce (partial merge-with +)))))
