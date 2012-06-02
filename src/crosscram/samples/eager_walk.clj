(ns crosscram.samples.eager-walk
  "Pick the first available space."
  (:require [crosscram.game :as cc]))

(defn make-move [game]
  (first (cc/available-moves (:board game))))
