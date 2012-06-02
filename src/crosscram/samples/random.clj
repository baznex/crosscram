(ns crosscram.samples.random
  "Pick a random available space."
  (:require [crosscram.game :as cc]))

(defn make-move [game]
  (rand-nth (cc/available-moves (:board game))))
