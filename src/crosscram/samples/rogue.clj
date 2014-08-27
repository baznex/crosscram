(ns crosscram.samples.rogue
  "Tries to steal as many spaces from the opponent as possible."
  (:require [crosscram.game :as game]))

(defn covered?
  "Returns a 1 if either cell of mm is contained in vv, otherwise 0."
  [mm vv]
  (if (some (set mm) vv) 1 0))

(defn coverage
   "Return the number of moves blocked by a domino."
   [xx xxs]
   (count (filter (partial covered? xx) xxs)))

 (defn make-move [game]
   (let [my-moves (game/available-moves (:board game))
         opp-moves (game/available-moves (game/transpose (:board game)))]
     (first (sort-by #(coverage % opp-moves) > my-moves))))
