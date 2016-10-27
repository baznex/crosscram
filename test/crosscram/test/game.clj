(ns crosscram.test.game
  (:use clojure.test
        clojure.template
        crosscram.game))

(deftest dominoes
  ;; validity
  (are [p e] (= (valid-domino? p) (boolean e))
       [[0 0] [1 0]] true
       [[2 9] [1 9]] true
       [[0 0] [0 1]] true
       ;; positional
       [[5 6] [5 6]] false
       [[1 0] [0 1]] false
       ;; values
       [[2 -1] [1 -1]] false
       [[0 0] [0 1.0]] false
       [[0 0] [nil 1]] false
       ;; dim
       [[0 0] [0 1] [0 2]] false
       [[0 0 1] [0 1 1]] false
       ;; coll type
       (list [0 0] [0 1]) false
       '[(0 1) [0 2]] false)
  ;; squares "abstraction"
  (is (= (domino-squares [[0 1] [1 1]]) [[0 1] [1 1]]))
  ;; orientation
  (is (horizontal? [[5 9] [5 10]]))
  (is (horizontal? [[5 10] [5 9]]))
  (is (not (horizontal? [[9 5] [10 5]])))
  ;; canonical order
  (is (= (canonical-domino [[0 1] [2 3]])
         (canonical-domino [[2 3] [0 1]]))))

(deftest boards-and-moves
  (let [empty (make-board [2 3])
        tile [[0 2] [1 2]]
        move0 (place-domino empty tile 0)]
    ;; impl
    (is (= empty [[nil nil nil] [nil nil nil]]))
    (is (= move0 [[nil nil 0] [nil nil 0]]))
    ;; impl - non-vertical
    (is (= (place-domino empty [[0 0] [0 1]] 5)
           [[5 5 nil] [nil nil nil]]))
    ;; api
    (is (= (board-size empty) [2 3]))
    (are [pos on?] (= (boolean (on-board? empty pos)) on?)
         [0 0] true
         [-1 0] false
         [0 -2] false
         [2 3] false
         [1 2] true
         [2 2] false)
    (is (= (lookup-square empty [0 2]) nil))
    (is (= (lookup-square move0 [0 2]) 0))
    (is (= (lookup-square move0 [1 2]) 0))
    (is (= (lookup-square move0 [1 1]) nil))))

(deftest inspection
  (is (= (set (map set (available-moves (place-domino (make-board [2 4])
                                                      [[1 2] [1 3]] 0))))
         #{#{[0 0] [0 1]} #{[0 1] [0 2]} #{[0 2] [0 3]} #{[1 0] [1 1]}}))
  (is (= (count (available-moves (place-domino (make-board [5 3])
                                               [[1 1] [2 1]] 0)))
         6))
  (is (can-move? (make-board [3 4])))
  (is (not (can-move? (make-board [1 1]))))
  (is (can-move? (place-domino (make-board [2 3]) [[0 0] [1 0]] 0)))
  (is (not (can-move? (place-domino (make-board [2 3]) [[0 1] [1 1]] 0))))
  (is (can-move? (place-domino (make-board [2 3]) [[0 2] [1 2]] 0)))
  (is (can-move? (place-domino (make-board [2 3]) [[0 1] [0 2]] 0))))

(deftest rotations
  (let [d [[1 2] [0 2]]]
    (is (= (rotate-domino d)
           (rotate-domino d 1)))
    (is (= (rotate-domino d 1) [[2 1] [2 0]]))
    (is (= (rotate-domino d 0) d))
    (is (= (rotate-domino d 1) (rotate-domino d -3)))
    (is (= (rotate-domino d 4) (rotate-domino d 18))))
  (let [in {:type :move :player-id 0 :move [[0 2] [1 2]] :duration 1}
        out {:type :move :player-id 0 :move [[2 0] [2 1]] :duration 1}]
    (is (= (rotate-event in) out))
    (is (= (rotate-event in) (rotate-event in 1)))
    (is (= (rotate-event in 0) in))
    (is (= (rotate-event in -1) (rotate-event in 3))))
  (let [p [[3 4] [2 4]]
        b57 (make-board [5 7])
        move0 (place-domino b57 p 0)]
    (is (= (place-domino b57 p 0)
           (rotate-board (place-domino (rotate-board b57 1)
                                       (rotate-domino p 1) 0)
                         1)))
    (is (= (rotate-board move0)
           (rotate-board move0 1)))
    (is (= (rotate-board move0 0) move0))
    (is (= (rotate-board move0 1) (rotate-board move0 -3)))
    (is (= (rotate-board move0 4) (rotate-board move0 18)))))

(deftest validity-moves
  (let [single (place-domino (make-board [4 4]) [[1 1] [2 1]] 0)]
    (do-template [p e]
                 (do (is (= (valid-move? single p) (boolean e)))
                     (if e
                       (is (place-domino single p 1)
                           (str "valid move " p))
                       (is (thrown? AssertionError
                                    (place-domino single p 1))
                           (str "invalid move " p))))
         [[0 0] [0 1]] true
         [[2 2] [2 3]] true
         ;; overlap
         [[1 1] [1 2]] false
         [[2 2] [2 1]] false
         ;; off board
         [[0 4] [0 3]] false
         [[4 1] [3 1]] false)))

(deftest games
  (is (= (make-game [2 3] 1)
         {:board (make-board [3 2]), :dims [3 2], :history [], :player-id 1}))
  (let [game-base (make-game [2 3] 0)
        move-0 [[0 0] [1 0]]
        event-0 {:type :move :player-id 0 :move move-0 :duration 1}
        game-0 (conj-event game-base event-0)
        move-1 [[0 1] [0 2]]
        event-1 {:type :move :player-id 1 :move move-1 :duration 1}
        game-1 (conj-event game-0 event-1)]
    (is (= (:board game-0) (place-domino (make-board [2 3]) move-0 0)))
    (is (= (:history game-0) [event-0]))
    (is (= (:history game-1) [event-0 event-1]))
    ;; rotations
    (is (= (rotate-game game-1 0) game-1))
    (is (= (rotate-game game-1)
           (rotate-game game-1 1)))
    (is (= (rotate-game game-1 -3) (rotate-game game-1 1)))
    (is (= (rotate-game game-1 18) (rotate-game game-1 0)))
    (is (= (rotate-game game-1 1)
           {:board (rotate-board (:board game-1) 1)
            :dims [3 2]
            :history [(rotate-event event-0 1) (rotate-event event-1 1)]
            :player-id 1}))
    ;; conj other events (left out: :invalid-move, :player-error)
    (let [end-0 {:type :cant-move :player-id 0}]
      (is (= (:board game-0) (:board (conj-event game-0 end-0))))
      (is (= (inc (count (:history game-0)))
             (count (:history (conj-event game-0 end-0))))))))
