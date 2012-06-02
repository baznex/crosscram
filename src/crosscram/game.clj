(ns crosscram.game
  "Game knowledge.

In this documentation, \"vector\" means any indexed, sequential collection.

A board is a 2-dimensional matrix. Create with #'board, access with
#'location-empty?

A cell on the board is addressed by a vector of [row column], zero-indexed.
These may also be called coordinates or squares.

A domino is a vector of two squares: [[r0 c0] [r1 c1]].
The order of the squares in a vector is not important, but the game engine
will not alter it.

A move is simply a domino.

A game history is a vector of moves. The index of each move is called
its ordinal; the move at index 0 was the first move. A new game will have
an empty history vector.

The board contains an alternate view of history. Each cell contains either
the ordinal (from the history vector) of the move that covered that square,
or nil if the square is open.

A gamestate value (which is provided to the bot) is a map of:
:board - a board value, as defined above
:dims - a vector of [row-count column-count]
:history - a history value, as defined above
:player-id - 0 or 1, indicating which player's view this is

This will sometimes simply be called a game value.")

;; Implementation details

;; In 2 dimensions:
;; - A board is a 2-level nesting of vectors. The top-level vector contains
;;   the row vectors.
;; - The order of the squares in a domino is not currently important to
;;   the game engine, but the history will contain the same ordering
;;   the bots provide.

;; TODO(timmc:2012-05-23) Should we canonicalize the order of the squares in
;; the domino on receipt from a bot?

;;;; Utils

(defn transpose [vv] (vec (apply map vector vv)))

;;;; Dominoes

(defn valid-domino?
  "Return true iff the value is a valid domino."
  [val]
  ;; TODO(timmc): loosen restriction on vector-ness and accept
  ;; anything seqable? Maybe write a "cleaner" fn that returns
  ;; a cleaned-up domnino, or nil.
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
coordinates produce ::outside-board value."
  [board square]
  (get-in board square ::outside-board))

(defn ^:internal set-square
  "Set the value of a square in a board."
  [board square val]
  (assoc-in board square val))

;;;; Moving

(defn place-domino
  "Place a domino on the board, assumed to be a valid move. The returned
board will have the specified move ordinal in the squares covered by the
domino."
  [board domino move-ord]
  (reduce #(set-square % %2 move-ord) board (domino-squares domino)))

(defn valid-move?
  "Checks if the domino may be placed on the board (contained
by board, does not overlap other pieces.) Assumes the domino is an
otherwise valid piece in either orientation."
  [board domino]
  (every? #(nil? (lookup-square board %)) (domino-squares domino)))

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

(defn move
  "Add a move (a domino) to a game."
  [game move]
  (let [ord (count (:history game))
        board (place-domino (:board game) move ord)]
    (-> game
        (assoc-in [:history ord] move)
        (assoc :board board))))

(defn rotate-game
  "Rotate a game from player 0's perspective to the specified player's
perspective. (Unary form defaults to 1.) Player ID will be used modulo 2.
NOTE: This updates the :player-id key as well."
  ([game]
     (rotate-game game 1))
  ([game player-id]
     (if (zero? (mod player-id 2))
       game
       {:board (rotate-board (:board game) player-id)
        :dims (let [[r c] (:dims game)] [c r])
        :history (vec (map rotate-domino (:history game)))
        :player-id (mod (+ (:player-id game) player-id) 2)})))
