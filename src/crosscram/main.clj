(ns crosscram.main
  (:require [crosscram.core :as cc]))

(defn load-player
  "Fetch a player map from a namespace, or nil. The map will contain:
:make-move - The make-move function."
  [ns-name]
  (binding [*out* *err*]
    (let [ns-sym (symbol ns-name)]
      (try (require ns-sym)
           (if-let [make-move (ns-resolve ns-sym 'make-move)]
             {:make-move (deref make-move)}
             (println "Error: No make-move function in namespace:" ns-name))
           (catch java.io.FileNotFoundException fnfe
             (println "Error: Could not find namespace:" ns-name))))))

(defn -main [player-a player-b rows columns num-games]
  (let [fns-a (load-player player-a)
        fns-b (load-player player-b)]
    (when (and fns-a fns-b)
      (let [scores (cc/play-symmetric
                    (cc/new-game (Integer/parseInt rows)
                                 (Integer/parseInt columns))
                    (:make-move fns-a)
                    (:make-move fns-b)
                    (Integer/parseInt num-games))]
        (println "Scores:" scores)))))
