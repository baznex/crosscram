(ns crosscram.test.samples.rogue
  (:use [clojure.test]
        [crosscram.samples.rogue]
        [crosscram.test.generators])
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer (defspec)]))

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
