(ns crosscram.demo
  "Human plays against a bot. Human plays horizontal.

The game is run in a background thread that blocks whenever it is the human's
turn to play. At that point, the GUI is unlocked and the human can select a
move. That action locks down the GUI again and delivers the move to the
blocking queue that the game loop is waiting on.

When each player fn is called by the game loop, it schedules a Swing event
to update the UI to the latest game state."
  (:require [crosscram.game :as g]
            [crosscram.engine :as cc])
  (:require [crosscram.samples.random :as bot])
  (:import
   [java.util.concurrent LinkedBlockingQueue]
   [javax.swing SwingUtilities UIManager
    JFrame JComponent]
   [java.awt Component Dimension Rectangle
    Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom Line2D Line2D$Double Rectangle2D Rectangle2D$Double]
   [java.awt.event MouseAdapter MouseEvent WindowAdapter WindowEvent]))

;;;; Settings

(def dim [5 8])

(def human-player-id 0) ;; FIXME: This is required to be 0 for now.
(def bot-player-id (mod (inc human-player-id) 2))

;;;; Util

(defn rotate
  "Rotate a collection right by non-negative n."
  [n coll]
  (let [size (count coll)
        m (mod (- size n) size)]
    (concat (drop m coll) (take m coll))))

(defn graphics-size
  "Get the size of a graphics object as [w h]."
  [^Graphics2D gfx]
  (let [cb (.getClipBounds gfx)]
    [(.width cb) (.height cb)]))

(let [serializer (agent nil)]
  (defn ts
    "Make a function \"thread-safe\" -- all calls to ts-produced fns will be
run on an agent thread, serially."
    [f]
    (fn [& args]
      (apply send serializer #(apply f (rest %&)) args))))

;;;; Threads

(def stopping? "Whether the program is stopping."
  (ref false))

(def bg-threads "Stack of background threads to shut down at the end."
  (ref ()))

(defn run-in-background "Run fn as named thread in background."
  [fn name]
  (dosync
   (when-not @stopping?
     (let [thread (Thread. fn name)]
       (.start thread)
       (alter bg-threads conj thread)))))

(defn stop-background-threads
  "Stop all background threads, in reverse order of when they were started."
  []
  (dosync
   (ref-set stopping? true)
   (while (seq @bg-threads)
     (let [[ft rt] @bg-threads]
       (ref-set bg-threads rt)
       (.stop ft)))))

(def run-via-main "Set to true at launch if started via -main."
  false)

(defn halt-program
  "Stop all background threads and allow program to exit."
  []
  (stop-background-threads)
  (when run-via-main
    (shutdown-agents)))

;;;; State

;; The game thread blocks on this until the UI drops in a value
(def human-moves "Blocking queue of human moves."
  (LinkedBlockingQueue.))

(def human-turn? "Atom of boolean: Human's turn?"
  (atom (zero? human-player-id)))

(def game "Atom: Game's most recent state, from player 0 perspective."
  (atom nil))

;;;; Game logic

(declare update-ui-game-state) ;; TODO: Is it possible to avoid this?

(defn horiz-move-for-cell
  [cell]
  (when-let [[r c] cell]
    [[r c] [r (inc c)]]))

(defn make-human-move
  "Block until human has moved, then return move."
  [g]
  (SwingUtilities/invokeLater (partial update-ui-game-state g))
  (reset! human-turn? true)
  ;; Block until human has moved
  (.take human-moves))

(defn complete-human-move
  "Complete the human's move."
  [cell]
  (.put human-moves (horiz-move-for-cell cell))
  (reset! human-turn? false))

(defn make-bot-move
  "Make the bot move."
  [g]
  (SwingUtilities/invokeLater (partial update-ui-game-state g))
  (bot/make-move g))

;;;; Geometry

(def ^:dynamic line-width 3)
(def ^:dynamic cell-width 30)

(defn canvas-cell-base
  "Base of cell (in either x or y) for row or column."
  [c]
  (+ line-width (* (+ cell-width line-width) c)))

(defn canvas-size
  "Get the canvas size in pixels as [w h] for [rows cols]."
  [[rows cols]]
  [(canvas-cell-base cols) (canvas-cell-base rows)])

(defn canvas-to-strip
  "Return the cell index in the row or column for a graphics distance `c` in
a strip of `n` cells -- or nil, if not in a cell."
  [n c]
  (let [guess (quot (- c line-width)
                    (+ cell-width line-width))
        upper (* (inc guess)
                 (+ cell-width line-width))]
    (when (and (<= 0 (- c line-width))
               (< c upper)
               (< guess n))
      guess)))

(defn canvas-pos-to-cell
  "Translate an x/y canvas graphics position to a Crosscram cell coordinate,
or nil if not on a cell."
  [[rows cols] x y]
  (let [r (canvas-to-strip rows y)
        c (canvas-to-strip cols x)]
    (when (and r c)
      [r c])))

;;;; Rendering - double-buffering and incremental updates

;; Drawn from player 0 perspective.

(def hover "Atom: Cell the mouse is hovering [r c] or nil if a) not hovering
or b) not the human's turn."
  (atom nil))

(def buffer-graphics (promise))
(def buffer-image (promise))

(defn fill-cell
  "Fill a game cell."
  [^Graphics2D gfx, [r c]]
  (let [xb (canvas-cell-base c)
        yb (canvas-cell-base r)]
    (.fill gfx (Rectangle. xb yb cell-width cell-width))))

(def next-event "Atom of ID of next event to render."
  (atom 0))

(defn render-new-events
  "Render any events we haven't seen already."
  [^Graphics2D gfx, events]
  (doseq [event-id (range @next-event (count events))
          :let [event (get events event-id)]]
    (when (= (:type event) :move)
      (.setColor gfx (Color. 50 50 50))
      (doseq [cell (g/domino-squares (:move event))]
        (fill-cell gfx cell)))
    (swap! next-event inc)))

(defn render-game
  "Render a game to a graphics object."
  [^Graphics2D gfx, game-state]
  (let [dim (:dims game-state)
        [gw gh] (canvas-size dim)]
    ;; bg
    (.setColor gfx (Color. 255 255 255))
    (.fill gfx (Rectangle. 0 0 gw gh))
    ;; grid
    (.setColor gfx (Color. 0 0 0))
    ;; row lines
    (.fill gfx (Rectangle. 0 0 gw line-width))
    (doseq [i (range 1 (inc (dim 0)))]
      (.fill gfx (Rectangle. 0 (* i (+ cell-width line-width))
                             gw line-width)))
    ;; col lines
    (.fill gfx (Rectangle. 0 0 line-width gh))
    (doseq [i (range 1 (inc (dim 1)))]
      (.fill gfx (Rectangle. (* i (+ cell-width line-width)) 0
                             line-width gh)))
    ;; existing moves
    (render-new-events gfx (:history game-state))))

;;;; GUI

(def ^JComponent canvas nil)
(def ^JFrame frame nil)

(defn update-ui-game-state
  "Receive the new game state from a player and render it."
  [g]
  (let [g (g/rotate-game g (:player-id g))]
    (reset! game g)
    (if-let [events (not-empty (:history g))]
      (render-new-events @buffer-graphics events)
      (render-game @buffer-graphics g))
    (.repaint canvas)))

(defn canvas-mouse-clicked
  [^MouseEvent e]
  (when (and (= (.getButton e) MouseEvent/BUTTON1) @human-turn?)
    (when-let [h @hover]
      (let [hover-move (horiz-move-for-cell h)]
        (when (g/valid-move? (:board @game) hover-move)
          (complete-human-move h)
          (reset! hover nil)
          (.repaint canvas))))))

(defn canvas-mouse-moved
  [^MouseEvent e]
  (when @human-turn?
    (let [old @hover
          new (canvas-pos-to-cell (:dims @game) (.getX e) (.getY e))]
      (when-not (= old new)
        (reset! hover new)
        (.repaint canvas)))))

(defn renderer
  [^Graphics2D gfx]
  ;; Unlikely corner-case: Rendering starts before game starts.
  (when (realized? buffer-image)
    (.drawImage gfx @buffer-image 0 0 nil))
  ;; hover
  (let [hover-move (horiz-move-for-cell @hover)]
    (when (and hover-move (g/valid-move? (:board @game) hover-move))
      (.setColor gfx (Color. 50 255 50))
      (doseq [cell hover-move]
        (fill-cell gfx cell)))))

;;;; Launch

(defn launch-gui
  "Create and display the GUI."
  [game-state]
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (alter-var-root
   #'canvas (constantly
             (let [mouse-adapter
                   (proxy [MouseAdapter] []
                     (mouseClicked [e] (canvas-mouse-clicked e))
                     (mouseMoved [e] (canvas-mouse-moved e)))
                   [cw ch] (canvas-size (:dims game-state))]
               (doto (proxy [JComponent] []
                       (paint [^Graphics2D g] (renderer g)))
                 (.setDoubleBuffered true)
                 (.addMouseListener mouse-adapter)
                 (.addMouseMotionListener mouse-adapter)
                 (.setPreferredSize (Dimension. cw ch))))))
  (alter-var-root
   #'frame (constantly
            (doto (JFrame. "Crosscram demo")
              (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
              (.addWindowListener (proxy [WindowAdapter] []
                                    (windowClosed [^WindowEvent we]
                                      (halt-program))))
              (.setResizable false)
              (.add canvas)
              (.pack))))
  ;; initialize double-buffering
  (let [[cw ch] (canvas-size (:dims game-state))]
    (deliver buffer-image (.createImage canvas cw ch)))
  (deliver buffer-graphics (.getGraphics @buffer-image))
  ;; ready!
  (.setVisible frame true))

(defn game-results
  "Describe the results of a finished game."
  [ending]
  {:pre [(not= :move (-> ending :history peek :type))]}
  (let [last-event (-> ending :history peek)
        human-won? (= (:player-id last-event) bot-player-id)
        turns (dec (count (:history ending)))]
    (case (:type last-event)
          :cant-move
          (format "The %s won after %d turn%s."
                  (if human-won? "human" "bot")
                  turns
                  (when (not= 1 turns) "s"))

          :invalid-move
          (format "The %s made an invalid move, and forfeited: %s"
                  (if human-won? "bot" "human")
                  (:return last-event))

          :player-error
          (if (contains? last-event :error)
            (format "The %s errored out, and forfeited: %s"
                    (if human-won? "bot" "human")
                    (:error last-event))
            (format "The %s returned a non-domino, and forfeited: %s"
                    (if human-won? "bot" "human")
                    (pr-str (:return last-event)))))))

(defn launch-game
  "Start a game loop with a bot and human player."
  [game-state]
  ;; human at index 0 so that rotation by ID makes sense
  (let [player-fns [make-human-move make-bot-move]
        ending (try (cc/play game-state
                             (vec (rotate human-player-id player-fns)))
                    (catch InterruptedException ie nil))] ;; requires clj > 1.3
    (when ending ;; if not interrupted
      ((ts println) "Game over:" (game-results ending))
      (SwingUtilities/invokeLater (partial update-ui-game-state ending)))))

(defn launch
  "Start application. Use this from the REPL, combined with :require +
:reload. (Does not shut down agents, unlike -main.)"
  []
  (let [initial-game (g/make-game dim 0)]
    (SwingUtilities/invokeLater (partial launch-gui initial-game))
    (run-in-background (partial launch-game initial-game) "Game loop")))

(defn -main
  "Start as stand-alone application. Takes no arguments."
  [& args]
  (alter-var-root #'run-via-main (constantly true))
  (launch)
  nil)
