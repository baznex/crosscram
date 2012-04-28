(ns crosscram.bots
  (:require [crosscram.core :as crosscram])
  (:require [crosscram.board :as board])
  (:require [clojure.core.match :as match])
  )

(defn brute-force-moves [game]
  (first (board/available-moves game)))

(defn random-moves [game]
  (rand-nth (board/available-moves game)))


(defn -main [rows columns num-games]
  (println (str "Scores: "
    (crosscram/play-symmetric
      (crosscram/new-game (Integer/parseInt rows) (Integer/parseInt columns) :horizontal )
      brute-force-moves
      random-moves
      (Integer/parseInt num-games)))))
