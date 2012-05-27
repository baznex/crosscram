(ns crosscram.samples.random
  "Pick a random available space."
  (:require [crosscram.board :as board]))

(defn make-move [game]
  (rand-nth (board/available-moves game)))
