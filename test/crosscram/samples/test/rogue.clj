(ns crosscram.samples.test.rogue
  (:use [clojure.test]
        [crosscram.samples.rogue])
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn gen-cell
  "Generates cells using the given generator(s) for their coordinates."
  ([] (gen-cell gen/pos-int))
  ([gen] (gen-cell gen gen))
  ([gen0 gen1] (gen/tuple gen0 gen1)))

(defn horiz-cell
  "Returns the cell's right neighbor, e.g. `(horiz-cell [0 0]) ; => [0 1]`."
  [cell] (update-in cell [1] + 1))

(defn gen-move
  "Returns a move generator moves based on the given cell generator."
  ([] (gen-move (gen-cell)))
  ([cell-gen]
   (gen/bind cell-gen
             (fn [cell]
               (gen/return cell)
               (gen/tuple (gen/return cell)
                          (gen/return (horiz-cell cell)))))))

(defn not-in-cells
  "Evalutes to `true` if `input` does not contain any cell from `cells`."
  [cells input]
  (not (some #{input} cells)))

(def covered-prop-simple
  (prop/for-all [mm (gen-move)]
                (= (covered? mm mm) 1)))

(def covered-prop
  (prop/for-all [mm (gen-move)]
                (let [n (gen/such-that (partial not-in-cells mm) (gen-cell))]
                  (=
                    (covered? mm [n (mm 0)])
                    (covered? mm [n (mm 1)])
                    1))))

(def not-covered-prop
  (prop/for-all [mm (gen-move)]
                (let [nn (gen/such-that #(not (some (set mm) %)) (gen-move))]
                  (= (covered? mm nn) 0))))
