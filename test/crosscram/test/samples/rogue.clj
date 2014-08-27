(ns crosscram.test.samples.rogue
  (:use [clojure.test]
        [crosscram.samples.rogue])
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

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
  (not (apply in-cells args)))

(defspec covered-simple 100
         (prop/for-all
           [vv (gen-move)]
           (= (covered? vv vv) 1)))

(defspec intersecting-moves-are-covered 100
         (prop/for-all
           [vv (gen-move)]
           (let [v (gen/such-that (partial not-contains-cell? vv) (gen-cell))]
             (= (covered? vv [v (vv 0)])
                (covered? vv [v (vv 1)])
                1))))

(defspec disjoint-moves-are-not-covered 100
         (prop/for-all
           [vv (gen-move)]
           (let [vv' (gen/such-that (partial not-contains-cell? vv) (gen-move))]
             (= (covered? vv vv') 0))))

(defspec coverage-test
         100
         (prop/for-all
           [vv (gen-move)]
           (let [vv-covered (gen/vector (gen/tuple (gen/elements vv) (gen-cell)))
                 vv-not-covered (gen/vector (gen/such-that (partial covered? vv) (gen-move)))
                 vvs (into vv-covered vv-not-covered)]
             (= (count vv-covered) (coverage vv vvs)))))
