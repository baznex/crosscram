(ns crosscram.main
  (:require [crosscram.engine :as engine]
            [crosscram.game :as game]))

(defn- load-player
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
      (let [dims [(Integer/parseInt rows) (Integer/parseInt columns)]
            num-games (Integer/parseInt num-games)
            scores (engine/play-symmetric
                    (game/make-game dims 0)
                    (:make-move fns-a)
                    (:make-move fns-b)
                    num-games)]
        (println "Scores:" scores)))))
