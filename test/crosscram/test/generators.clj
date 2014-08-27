(ns crosscram.test.generators
  (:require [clojure.test.check.generators :as gen]))

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

(defn contains-cell?
  "Evalutes to `true` if a cell equal to `v` is present in `vvs`."
  [vs v]
  {:pre [(integer? (first v)) (not (empty? v))]}
  (some #{v} vs))

(defn not-contains-cell?
  "Inverse of `contains-cell?`"
  [& args]
  (not (apply contains-cell? args)))
