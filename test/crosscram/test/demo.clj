(ns crosscram.test.demo
  (:use crosscram.demo
        clojure.test))

(deftest util
  (is (= (rotate 0 [1 2 3 4 5]) [1 2 3 4 5]))
  (is (= (rotate 2 [1 2 3 4 5]) [4 5 1 2 3]))
  (is (= (rotate -1 [1 2 3 4 5]) [2 3 4 5 1])))

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
