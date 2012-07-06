(ns crosscram.game
  "Game knowledge.

## Terms

In this documentation, \"vector\" means any indexed, sequential collection.

## Boards

A board is a 2-dimensional matrix. Create with #'board, access with
#'location-empty?

A cell on the board is addressed by a vector of [row column], zero-indexed.
These may also be called coordinates or squares.

## Dominoes

A domino is a vector of two squares: [[r0 c0] [r1 c1]].
The order of the squares in a vector is not important, but the game engine
will not alter it.

## Moves

A move is simply a domino. A \"valid move\" is in reference to a board,
whereas a domino may be judged valid or not without reference to anything.

## History

A game history is a vector of events. An event is a map of :type (:move,
:invalid-move, :player-error, :cant-move) and possibly other keys
depending on :type. A :move is a valid move that can be placed on the current
board; an :invalid-move is a valid domino that cannot be played on this board,
including mis-oriented dominoes. A :player-error is a throw or a returned value
that is not a domino. :cant-move occurs when the game is over -- the player
has not been consulted. A completed game's history ends with a non-:move event.

* :player-id - 0 or 1
* :duration - bot's elapsed time (nanoseconds), for all but :cant-move
* :move - played domino (canonicalized), for :move or :invalid-move
* :error - with :player-error, the thrown error (if applicable)
* :return - with :player-error, the returned value (if applicable)

The index of each event is called its ordinal; the event at index 0 was
the first event. A new game will have an empty history vector.

The board contains an alternate view of history. Each cell contains either
the ordinal (from the history vector) of the move that covered that square,
or nil if the square is open. Future work may add additional cell value types,
but open squares will remain logical-false for the foreseeable future.

## Gamestate

A gamestate value (which is provided to the bot) is a map of:
:board - a board value, as defined above
:dims - a vector of [row-count column-count]
:history - a history value, as defined above
:player-id - 0 or 1, indicating which player's view this is
:over? - indicates the game is over and the last history event records this

This will sometimes simply be called a game value.")

;; Implementation details

;; In 2 dimensions:
;; - A board is a 2-level nesting of vectors. The top-level vector contains
;;   the row vectors.
;; - The order of the squares in a domino is canonically in low to high
;;   order along the x axis. Bots are allowed to return dominoes in either
;;   order, but some methods may declare a need for canonical order. The
;;   history will contain canonical dominoes.

;;;; Utils

(defn transpose [vv] (vec (apply map vector vv)))

;;;; Dominoes

(defn valid-domino?
  "Return true iff the value is a valid domino."
  [val]
  (let [vec2? #(and (sequential? %)
                    (associative? %)
                    (counted? %)
                    (= (count %) 2)) ;; TODO dimension-agnostic
        natural? #(and (integer? %) (<= 0 %))
        xor2 #(or (and %1 (not %2))
                  (and %2 (not %1)))]
    (and (vec2? val)
         (every? vec2? val)
         (every? natural? (apply concat val))
         (let [[[r0 c0] [r1 c1]] val]
           (xor2 (= (Math/abs (long (- r0 r1))) 1)
                 (= (Math/abs (long (- c0 c1))) 1))))))

(defn canonical-domino
  "Answer a domino such that for all representations D1,D2 of a
domino, (= (canonical-domino d1) (canonical-domino d2))"
  [domino]
  (into [] (sort domino)))

(defn domino-squares
  "Return a sequence of the coordinates occupied by a valid domino."
  [domino]
  (seq domino))

(defn horizontal?
  "Checks if the domino is horizontal (that is, the second coordinates
differ by 1, but the first coordinates are equal.) Assumes domino is
otherwise valid."
  [domino]
  (let [[[r0 c0] [r1 c1]] domino]
    (and (= r0 r1) (= 1 (Math/abs (long (- c0 c1)))))))

(defn rotate-domino
  "Rotate a domino from player 0's perspective to the specified player's
perspective. (Unary form defaults to 1.) Player ID will be used modulo 2."
  ([domino]
     (rotate-domino domino 1))
  ([domino player-id]
     (if (zero? (mod player-id 2))
       domino
       (vec (map (comp vec reverse) domino)))))

(defn posint?
  "Return logical true if given a positive integer."
  [x]
  (and (integer? x) (pos? x)))

;;;; History

(defn rotate-event
  "Rotate a history event from player 0's perspective to the specified player's
perspective. (Unary form defaults to 1.) Player ID will be used modulo 2."
  ([event]
     (rotate-event event 1))
  ([event player-id]
     {:pre [(:type event)]}
     (if (#{:move :invalid-move} (:type event))
       (update-in event [:move] rotate-domino player-id)
       event)))

;;;; Boards

(defn make-board
  "Given a dimensions vector of [rows, columns], generate an empty board.
Both dimensions must be positive integers."
  [[rows columns]]
  {:pre [(posint? rows) (posint? columns)]}
  (vec (repeat rows (vec (repeat columns nil))))) ;; TODO dimension-agnostic

(defn board-size
  "Get the board size as a vector of row, column."
  [board]
  [(count board) (count (first board))])

(defn on-board?
  "Test if a row/column coordinate pair is a board coordinate."
  [board [rv cv]]
  (let [[rows cols] (board-size board)]
    (and (<= 0 rv (dec rows))
         (<= 0 cv (dec cols)))))

(defn rotate-board
  "Rotate a board from player 0's perspective to the specified player's
perspective. (Unary form defaults to 1.) Player ID will be used modulo 2."
  ([board]
     (rotate-board board 1))
  ([board player-id]
     {:pre [(vector? board)]}
     (if (zero? (mod player-id 2))
       board
       (transpose board))))

(defn available-moves
  "Generate a lazy seq of all possible horizontal moves. To get opponent
moves, rotate the board first."
  [board]
  (for [[r row] (map-indexed vector board)
        found (keep-indexed (fn [c pair] (when (= [nil nil] pair)
                                           [[r c] [r (inc c)]]))
                            (partition 2 1 row))]
    found))

(defn can-move?
  "Return logical true if there is at least one place for a horizontal move."
  [board]
  (boolean (seq (available-moves board))))

(defn lookup-square
  "Discover if a board position is empty. Given a location [r c] on a board,
return the ordinal of the move that filled it, or nil if empty. Invalid
coordinates produce :crosscram.game/outside-board value."
  [board square]
  (get-in board square ::outside-board))

(defn ^:internal set-square
  "Set the value of a square in a board."
  [board square val]
  (assoc-in board square val))

;;;; Moving

(defn valid-move?
  "Checks if the domino may be placed on the board (contained
by board, does not overlap other pieces.) Assumes the domino is an
otherwise valid piece in either orientation."
  [board domino]
  (every? #(nil? (lookup-square board %)) (domino-squares domino)))

(defn place-domino
  "Place a domino on the board, assumed to be a valid move. The returned
board will have the specified move ordinal in the squares covered by the
domino."
  [board domino move-ord]
  {:pre [(valid-move? board domino)]}
  (reduce #(set-square % %2 move-ord) board (domino-squares domino)))

;;;; Games

(defn make-game
  "Given the dimensions of a board (rows, columns) create a blank game
for the indicated player. The player ID may be 0 or 1."
  [dims player-id]
  (let [players (count dims)
        dims (vec (take players (drop player-id (cycle dims))))]
    {:board (make-board dims)
     :dims dims
     :history []
     :player-id player-id}))

(defn conj-event
  "Apply a history event to a game. Requires canonicalized dominoes."
  [game event]
  (let [event-ord (count (:history game))
        updated (if (= :move (:type event))
                  (update-in game [:board] place-domino (:move event) event-ord)
                  game)]
    (update-in updated [:history] conj event)))

(defn rotate-game
  "Rotate a game from player 0's perspective to the specified player's
perspective. (Unary form defaults to 1.) Player ID will be used modulo 2.
NOTE: This updates the :player-id key as well."
  ([game]
     (rotate-game game 1))
  ([game player-id]
     {:pre [(:board game)]}
     (if (zero? (mod player-id 2))
       game
       {:board (rotate-board (:board game) player-id)
        :dims (let [[r c] (:dims game)] [c r])
        :history (vec (map rotate-event (:history game)))
        :player-id (mod (+ (:player-id game) player-id) 2)})))

(defn winner
  "Returns the ID of the winning player of a finished game."
  [game]
  {:pre [(:over? game)]}
  (mod (count (:history game)) 2))
