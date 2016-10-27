(ns crosscram.samples.move-advantage
  "Maximizes the difference between the number of moves we'll have left and the
  number of moves our opponent will have left."
  (:require [crosscram.game :as cc]))

(defn evaluate
  [{:keys [board player-id] :as game}]
  (let [our-moves (count (cc/available-moves board))
        their-moves (-> board
                      (cc/rotate-board 1)
                      cc/available-moves
                      count)]
    (- our-moves their-moves)))

(defn make-move
  [game]
  (let [board (:board game)
        possible-states (map (partial cc/move game)
                             (cc/available-moves board))]
    (->> (for [gs possible-states]
           ((juxt (comp last :history) evaluate) gs))
      (reduce (fn [[_ best-val :as best] [_ val :as candidate]]
                (if (> val best-val)
                  candidate
                  best)))
      first)))
