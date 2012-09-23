(ns crosscram.test.demo
  (:use crosscram.demo
        clojure.test))

(deftest coordinate-transforms
  (binding [line-width 3
            cell-width 20]
    (are [n c cell] (= (canvas-to-strip n c) cell)
         ;; before first cell
         10 0 nil
         10 2 nil
         ;; bounds of first cell
         10 3 0
         10 22 0
         ;; second line
         10 23 nil
         10 25 nil
         ;; another cell
         10 26 1
         ;; beyond the end of the strip
         1 22 0
         1 23 nil
         1 26 nil)
    (is (= (canvas-pos-to-cell [2 3] 5 28) [1 0]))))
