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
  (:require [crosscram.samples.reserves-move :as bot])
  (:import
   [java.util.concurrent LinkedBlockingQueue]
   [javax.swing SwingUtilities UIManager
    JFrame JComponent]
   [java.awt Component Dimension Rectangle
    Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom Line2D Line2D$Double Rectangle2D Rectangle2D$Double]
   [java.awt.event MouseAdapter MouseEvent WindowAdapter WindowEvent]))

;;;; Util

(defn rotate
  "Rotate a collection right by non-negative n."
  [n coll]
  {:pre [(or (pos? n) (zero? n))]}
  (concat (drop n coll) (take n coll)))

(defn graphics-size
  "Get the size of a graphics object as [w h]."
  [^Graphics2D gfx]
  (let [cb (.getClipBounds gfx)]
    [(.width cb) (.height cb)]))

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

;;;; Settings

(def dim [5 8])

(def human-player-id 0)
(def bot-player-id (mod (inc human-player-id) 2))

;;;; State

;; The game thread blocks on this until the UI drops in a value
(def human-moves "Blocking queue of human moves."
  (LinkedBlockingQueue.))

(def human-turn? "Atom of boolean: Human's turn?"
  (atom (zero? human-player-id)))

(def game "Atom: Game's most recent state, from player 0 perspective."
  (atom nil))

(def hover "Atom: Cell the mouse is hovering [r c] or nil if a) not hovering
or b) not the human's turn."
  (atom nil))

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

;;;; Game logic

(declare update-ui-game-state) ;; TODO: Is it possible to avoid this?

(defn make-human-move
  "Block until human has moved, then return move."
  [g]
  (SwingUtilities/invokeLater #(update-ui-game-state g human-player-id))
  (reset! human-turn? true)
  ;; Block until human has moved
  (.take human-moves))

(defn complete-human-move
  "Complete the human's move."
  [move]
  (.put human-moves move)
  (reset! human-turn? false))

(defn make-bot-move
  "Make the bot move."
  [g]
  (SwingUtilities/invokeLater #(update-ui-game-state g bot-player-id))
  (bot/make-move g))

(defn launch-game
  "Start a game loop with a bot and human player."
  [game-state]
  ;; human at index 0 so that rotation by ID makes sense
  (let [player-fns [make-human-move make-bot-move]]
    (cc/play game-state
             (vec (rotate human-player-id player-fns)))))

(defn horiz-move-for-cell
  [cell]
  (when-let [[r c] cell]
    [[r c] [r (inc c)]]))

;;;; Rendering - double-buffering and incremental updates

;; Drawn from player 0 perspective.

(def buffer-graphics (promise))
(def buffer-image (promise))

(defn fill-cell
  "Fill a game cell."
  [^Graphics2D gfx, [c r]]
  (let [xb (canvas-cell-base c)
        yb (canvas-cell-base r)]
    (.fill gfx (Rectangle. xb yb cell-width cell-width))))

(defn render-event
  "Render an event."
  [^Graphics2D gfx, event]
  (when (= (:type event) :move)
    (.setColor gfx (Color. 50 50 50))
    (doseq [cell (g/domino-squares (:move event))]
      (fill-cell gfx cell))))

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
    (doseq [e (:history game-state)]
      (render-event gfx e))))

(defn update-ui-game-state
  "Receive the new game state from a player and render it."
  [g player-id]
  (reset! game (g/rotate-game g player-id))
  (if-let [events (not-empty (:history g))]
    (render-event @buffer-graphics (peek events))
    (render-game @buffer-graphics g)))

;;;; GUI

(def ^JComponent canvas nil)
(def ^JFrame frame nil)

(defn canvas-mouse-clicked
  [^MouseEvent e]
  (when @human-turn?
    (when-let [h @hover]
      (when (= (.getButton e) MouseEvent/BUTTON1)
        (complete-human-move h)
        (reset! hover nil)
        (.repaint canvas)))))

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
    ;;TODO: Move valid-move logic to event handler to avoid unecessary repaint
    (when (and hover-move (g/valid-move? (:board @game) hover-move))
      (.setColor gfx (Color. 50 255 50))
      (doseq [cell hover-move]
        (fill-cell gfx cell)))))

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
                                      (stop-background-threads))))
              (.setResizable false)
              (.add canvas)
              (.pack))))
  ;; initialize double-buffering
  (let [[cw ch] (canvas-size (:dims game-state))]
    (deliver buffer-image (.createImage canvas cw ch)))
  (deliver buffer-graphics (.getGraphics @buffer-image))
  ;; ready!
  (.setVisible frame true))

(defn -main
  "Start application. Takes no arguments."
  [& args]
  (let [initial-game (g/make-game dim 0)]
    (SwingUtilities/invokeLater (partial launch-gui initial-game))
    (run-in-background (partial launch-game initial-game) "Game loop")))
