(ns crosscram.samples.biggest-space
  "Pick a space that has the widest open area around it. Doesn't do
particularly well against random.clj. Feel free to hack on this bot.
Some of these utility functions might be useful."
  (:require [crosscram.game :as cc]))

(defn canonical
  "Order a horizontal piece in left to right square order."
  [piece]
  (let [[[row c0] [_ c1]] piece
        left (min c0 c1)]
    [[row left] [row (inc left)]]))

(defn neighborhood
  "Get board values for a neighborhood centered on a possible move with
`radius` squares in each direction. Returns a vector of row vectors
containing the values from the board."
  [board radius move]
  (let [[[row left] _] (canonical move)]
    (vec (for [y (range (- row radius) (+ row radius 1))]
           (vec (for [x (range (- left radius) (+ left radius 2))]
                  (cc/lookup-square board [y x])))))))

(defn count-nil
  "Given a neighborhood, count the nils."
  [hood]
  (count (filter nil? (apply concat hood))))

(defn pick
  "Pick a random move from the highest-score cohort."
  [candidates]
  (->> candidates
       (sort-by :score >)
       (partition-by :score)
       (drop-while empty?)
       first
       rand-nth
       :move))

(def radius 3)

(defn make-move
  [game]
  (let [board (:board game)
        candidates (for [m (cc/available-moves board)]
                     {:move m
                      :score (count-nil (neighborhood board radius m))})]
    (pick candidates)))
