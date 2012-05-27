(ns crosscram.board
  (:require [clojure.core.match :as match]))

;;;
;;; Validation Functions
;;;

(defn valid-empty-space? [board [r c]]
  (cond
    (not (< -1 r (count board))) false
    (not (< -1 c (count (nth board r)))) false
    :else (nil? (nth (nth board r) c))))

(defn valid-pair? [[a b] [c d]] 
  (cond (= a c) (= 1 (Math/abs (- b d)))
        (= b d) (= 1 (Math/abs (- a c)))
        :else false))

(defn horizontal-pair? [[a b] [c d]] 
  (if (= a c)
    (= 1 (Math/abs (- b d)))
    false))

(defn vertical-pair? [[a b] [c d]] 
  (if (= b d)
    (= 1 (Math/abs (- a c)))
    false))

(defn- two-nil? [coll]
  (some (partial = [nil nil]) (partition 2 1 coll)))

(defn can-play-horizontal? [board]
  (boolean
    (some identity (map two-nil? board))))

(defn can-play-vertical? [board]
  (can-play-horizontal? (apply map vector board)))

;;;
;;; Convenience functions for determining possible moves
;;;

(defn two-d-get [coll [a b]]
  ; TODO should probably not use get; it returns nil even if out-of-bounds (use nth instead)
  (get (get coll a) b))

(defn location-empty? [board pos-a pos-b]
  (and (nil? (two-d-get board pos-a))
       (nil? (two-d-get board pos-b))))

(defn- generate-horizontal-for-row [columns row]
  (take (- columns 1)
        (for [x (range columns)]
          [[row x] [row  (+ 1 x)]])))

(defn- generate-vertical-for-column [rows column]
  (take (- rows 1)
        (for [x (range rows)]
          [[x column] [(+ 1 x) column]])))

(defn- generate-horizontal [rows columns]
  (mapcat (partial generate-horizontal-for-row columns)
          (range rows)))

(defn- generate-vertical [rows columns]
  (mapcat (partial generate-vertical-for-column rows)
          (range columns)))

(defn generate-all-opponents-moves [game]
  (generate-vertical (:rows game) (:columns game)))

(defn generate-all-my-moves [game]
  (generate-horizontal (:rows game) (:columns game)))

(defn available-opposing-moves [game]
  (filter
    (fn [arg] (location-empty? (:board game) (first arg) (last arg)))
    (generate-all-opponents-moves game)))

(defn available-moves [game]
  (filter
    (fn [arg] (location-empty? (:board game) (first arg) (last arg)))
    (generate-all-my-moves game)))

;;;
;;; Board-update functions
;;;

(defn board [rows columns]
  (vec (repeat rows
               (vec
                 (take columns (repeat nil))))))

(defn add-piece [board piece a b]
  (-> board
    (assoc-in a piece)
    (assoc-in b piece)))

(defn transpose [board]
  (vec (apply map vector board)))