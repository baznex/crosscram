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

(defn coverage
   "Return the number of moves blocked by a domino."
   [mm enemy-moves]
   (apply + (map #(covered? mm %) enemy-moves)))

 (defn make-move [game]
   (let [my-moves (game/available-moves (:board game))
         enemy-moves (game/available-moves (game/transpose (:board game)))]
     (first (sort-by #(coverage % enemy-moves) > my-moves))))
