(ns crosscram.samples.eager-walk
  "Pick the first available space."
  (:require [crosscram.board :as board]))

(defn make-move [game]
  (first (board/available-moves game)))
