(ns crosscram.board
  (:require [clojure.core.match :as match]))

;;
;; Validation Functions
;;

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
  (= true (reduce (fn [acc elem]
                    (match/match [acc elem]
                                 [true _] true
                                 [0 nil] 1
                                 [1 nil] true
                                 [_ _] 0))
                  0 coll)))

(defn can-play-horizontal? [board]
  (boolean
    (some identity (map two-nil? board))))

(defn can-play-vertical? [board]
  (can-play-horizontal? (apply map vector board)))

;;
;; Convenience functions for determining possible moves
;;

(defn two-d-get [coll [a b]]
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

(defn generate-possible-moves [game]
  (match/match (:next-player game)
                :horizontal (generate-horizontal (:rows game) (:columns game))
                :vertical (generate-vertical (:rows game) (:columns game))))

(defn available-moves [game]
  (filter
    (fn [arg] (location-empty? (:board game) (first arg) (last arg)))
    (generate-possible-moves game)))

;;
;; Board-update functions
;;

(defn board [rows columns]
  (vec (repeat rows
               (vec
                 (take columns (repeat nil))))))

(defn add-piece [board piece a b]
  (-> board
    (assoc-in a piece)
    (assoc-in b piece)))
