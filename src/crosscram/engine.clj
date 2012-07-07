(ns crosscram.engine
  "Game engine.

A bot is a function of [game -> domino].

The returned domino must be horizontal, meaning the row coordinates
are equal but the column coordinates differ by 1 Every player plays
horizontal, and sees the other player as playing vertical. (The
game engine gives the second player a transposed view of the board.)

If a bot returns an invalid move or an invalid domino or simply throws
an exception, the bot will lose that game."
  (:require [crosscram.game :as game]))

;; TODO(timmc:2012-05-24) In a 3-player game, the winner is the last
;; player standing after successive elimination.

;; TODO: Strip metadata from returned dominoes. Player could be storing state
;; there or otherwise be up to no good.

(defn score [game1 game2]
  (let [pair [game1 game2]]
    (cond
      (= pair [0 1]) {:bot-a 1 :bot-b 0 :draws 0}
      (= pair [1 0]) {:bot-a 0 :bot-b 1 :draws 0}
      :else          {:bot-a 0 :bot-b 0 :draws 1})))

(defn- timeit
  "Run a thunk and deliver the elapsed time in nanoseconds to a promise."
  [thunk prm]
  {:pre [(cast clojure.lang.IPending prm), (not (realized? prm))]}
  ;; TODO: Is this the most appropriate timer?
  (let [start (System/nanoTime)]
    (try (thunk)
         (finally
          (let [end (System/nanoTime)]
            (deliver prm (- end start)))))))

(defn- result->event
  "Given a bot-call result and an event base (:duration and :player-id filled
in), produce an event."
  [game result evbase]
  (if (contains? result :value)
    (let [val (:value result)]
      (if (game/valid-domino? val)
        (let [candom (game/canonical-domino val)]
          (if (game/valid-move? (:board game) candom)
            (assoc evbase :type :move, :move candom)
            (assoc evbase :type :invalid-move, :move candom)))
        (assoc evbase :type :player-error, :return val)))
    (assoc evbase :type :player-error, :error (:error result))))

(defn ^:internal call-bot
  "Ask the bot for a move. (Assumes a move is available.)  Returns an event."
  [game player-fn]
  {:pre [(:board game), (fn? player-fn)]}
  (let [timer (promise)
        ;; TODO(security)(timmc:2012-07-07) validate and canonicalize
        ;; the domino inside the try/catch. Malicious bots could use
        ;; reify to make something really ugly.
        result (try {:value (timeit #(player-fn game) timer)}
                    (catch Exception e {:error e}))]
    (result->event game result {:player-id (:player-id game)
                                :duration @timer})))

(defn ^:internal play-step
  "Step the game forward by one move. Rotates the board to be ready for
the next player, or returns to original orientation when game is over."
  [game player-fns]
  (let [next-player (:player-id game)
        event (if (game/can-move? (:board game))
                (call-bot game (get player-fns (mod next-player 2)))
                {:type :cant-move, :player-id next-player})
        over? (not= (:type event) :move)
        rotate-by (if over? (- next-player) 1)]
    (-> game
        (game/conj-event event)
        (game/rotate-game rotate-by)
        (assoc :over? over?))))

(defn play
  "Play a game. Returns the final game state, in original orientation."
  [game player-fns]
  {:pre [(vector? player-fns)]}
  (first (drop-while (complement :over?)
                     (iterate #(play-step % player-fns)
                              game))))

(defn play-symmetric
  "Play two bots in on a board of the given dimensions for a set number of
rounds. Return a winnings count map with keys :bot-a, :bot-b, and :draws."
  [dims [bot-a bot-b] num-rounds]
  ;; TODO: Define a player type, and pass those in, not bot fns
  (let [compete #(game/winner (play (game/make-game dims 0) [%1 %2]))]
    (loop [scoreboard {}]
      (if (<= num-rounds (apply + (vals scoreboard)))
        scoreboard
        (let [g1 (compete bot-a bot-b)
              g2 (compete bot-b bot-a)]
          (recur (merge-with + scoreboard (score g1 g2))))))))
