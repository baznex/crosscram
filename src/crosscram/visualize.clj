(ns crosscram.visualize
  (:require [hiccup.core :as hiccup]))

(defn space-separated [coll]
  (interpose " " coll))

(defn class-name-from-keywords [& args]
  (apply str (space-separated (map name args))))

(defn piece [shape player]
  [:span {:class (class-name-from-keywords shape player :piece)} "0"])

(defn piece-from-map [p]
  (apply piece (map p [:shape :player])))

(defn row [r]
  [:div (for [p r] (piece-from-map p))])

(defn rows [board]
  (for [r board] [:div {:class "row"}
                  (row r)]))

(defn html-board [board]
  [:div {:class "board"} (rows board)])
