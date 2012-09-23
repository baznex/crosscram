(ns crosscram.demo
  "Human plays against a bot. Human plays horizontal."
  (:require [crosscram.game :as g])
  (:require [crosscram.samples.windowshade-rand :as bot])
  (:import
   [javax.swing SwingUtilities UIManager
    JFrame JComponent]
   [java.awt Component Dimension Rectangle
    Graphics2D RenderingHints Color BasicStroke]
   [java.awt.geom Line2D Line2D$Double Rectangle2D Rectangle2D$Double]
   [java.awt.event MouseAdapter MouseEvent]))

;;;; Settings

(def dim [5 8])
(def human-goes-first true)

;;;; State

(def game "Atom of the gamestate."
  (atom (g/make-game dim 0)))

(def hover "Atom of the cell the mouse is hovering [x y] or nil."
  (atom nil))

;;;; GUI

(def ^JComponent canvas nil)
(def ^JFrame frame nil)

(def ^:dynamic line-width 3)
(def ^:dynamic cell-width 30)

(defn preferred-canvas-size
  [[rows cols]]
  (Dimension. (+ line-width (* (+ cell-width line-width) cols))
              (+ line-width (* (+ cell-width line-width) rows))))

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

;;;; ====

(defn canvas-mouse-clicked
  [e])

(defn canvas-mouse-moved
  [^MouseEvent e]
  (let [old @hover
        new (canvas-pos-to-cell dim (.getX e) (.getY e))]
    (when-not (= old new)
      (reset! hover new)
      (.repaint canvas))))

(defn renderer
  [^Graphics2D g]
  (let [canvas-size (preferred-canvas-size dim)
        hover-move (when-let [h @hover] [h [(h 0) (inc (h 1))]])]
    ;; bg
    (.setColor g (Color. 255 255 255))
    (.fill g (Rectangle. canvas-size))
    ;; grid
    (.setColor g (Color. 0 0 0))
    ;; row lines
    (.fill g (Rectangle. 0 0 (.getWidth canvas-size) line-width))
    (doseq [i (range 1 (inc (dim 0)))]
      (.fill g (Rectangle. 0 (* i (+ cell-width line-width))
                           (.getWidth canvas-size) line-width)))
    ;; col lines
    (.fill g (Rectangle. 0 0 line-width (.getHeight canvas-size)))
    (doseq [i (range 1 (inc (dim 1)))]
      (.fill g (Rectangle. (* i (+ cell-width line-width)) 0
                           line-width (.getHeight canvas-size))))
    ;; hover
    (when (and hover-move (g/valid-move? (:board @game) hover-move))
      (.setColor g (Color. 50 255 50))
      (doseq [[r c] hover-move]
        (let [xb (+ line-width (* c (+ line-width cell-width)))
              yb (+ line-width (* r (+ line-width cell-width)))]
          (.fill g (Rectangle. xb yb cell-width cell-width)))))))

(defn launch
  "Create and display the GUI."
  []
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (alter-var-root
   #'canvas (constantly
             (let [mouse-adapter
                   (proxy [MouseAdapter] []
                     (mouseClicked [e] (canvas-mouse-clicked e))
                     (mouseMoved [e] (canvas-mouse-moved e)))]
               (doto (proxy [JComponent] []
                       (paint [^Graphics2D g] (renderer g)))
                 (.setDoubleBuffered true)
                 (.addMouseListener mouse-adapter)
                 (.addMouseMotionListener mouse-adapter)
                 (.setPreferredSize (preferred-canvas-size (:dims @game)))))))
  (alter-var-root
   #'frame (constantly
            (doto (JFrame. "Crosscram demo")
              (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
              (.setResizable false)
              (.add canvas)
              (.pack))))
  (.setVisible frame true))

(defn -main
  "Start application. Takes no arguments."
  [& args]
  (SwingUtilities/invokeLater launch))
