(ns crosscram.samples.windowshade-rand
  "Algorithm by J. Hunter Heinlen: Strongly prefer even-index rows
and weakly prefer even-index columns. If played by iteself, this bot will
fill in alternating rows (leaving space for itself) and then play the
remaining rows."
  (:require [crosscram.game :as game]))

(def separate (juxt filter remove))

(defn make-move [game]
  (let [all (game/available-moves game)
        [e_ o_] (separate (comp even? ffirst) all)
        [ee eo] (separate #(even? (second (first %))) e_)
        [oe oo] (separate #(even? (second (first %))) o_)]
    (rand-nth (first (drop-while empty? [ee eo oe oo])))))
