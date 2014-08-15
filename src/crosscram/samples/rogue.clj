(ns crosscram.samples.rogue
  "Tries to steal as many spaces from the opponent as possible."
  (:require [crosscram.game :as game]))

(defn covered?
  "Returns a 1 if either cell of mm is contained in vv, otherwise 0."
  [[mm0 mm1] vv]
  (if (or (some #{mm0} vv)
          (some #{mm1} vv))
      1
      0))

; (defn coverage
;   "Return the number of moves blocked by a domino."
;   [mm enemy-moves]
;   (+ (map #(covered? mm %) enemy-moves)))

; (defn make-move [game]
;   (let [id (:player-id game)
;         my-wtd-moves (map coverage (game/available-moves (:board game)))
;         enemy-moves (game/available-moves (game/rotate-board (:board game) id))]
;     (first (sort #(coverage % enemy-moves) my-wtd-moves))))
