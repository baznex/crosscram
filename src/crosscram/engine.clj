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

(defn score [game1 game2]
  (let [pair [game1 game2]]
    (cond
      (= pair [0 1]) {:bot-a 1 :bot-b 0 :draws 0}
      (= pair [1 0]) {:bot-a 0 :bot-b 1 :draws 0}
      :else          {:bot-a 0 :bot-b 0 :draws 1})))

(defn- timeit
  "Run a thunk and deliver the elapsed time in nanoseconds to a promise."
  [thunk prm]
  ;; TODO: Is this the most appropriate timer?
  {:pre [(cast clojure.lang.IPending prm), (not (realized? prm))]}
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

(defn play-step
  "Step the game forward by one move. "
  [game player-fns]
  (let [next-player (:player-id game)
        evbase {:player-id next-player}]
    (if (game/can-move? (:board game))
      (let [player-fn (get player-fns (mod next-player 2))
            timer (promise)
            result (try
                     {:value (timeit #(player-fn game) timer)}
                     (catch Exception e {:error e}))
            event (result->event game result (assoc evbase :duration @timer))]
        (-> game
            (game/conj-event event)
            (game/rotate-game)
            (assoc :over? (not (= :move (:type event))))))
      (-> game
          (game/conj-event (assoc evbase :type :cant-move))
          (game/rotate-game (- next-player))
          (assoc :over? true)))))

(defn play
  "Play a game. Returns the final game state, in original orientation."
  [game player-fns]
  {:pre [(vector? player-fns)]}
  (first (drop-while (complement :over?) (iterate #(play-step % player-fns)
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
