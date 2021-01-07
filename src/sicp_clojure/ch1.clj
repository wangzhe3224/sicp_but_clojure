(ns sicp_clojure.ch1)

;; Exercise 1.2
(def ex2 
    (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
       (* 3 (- 6 2) (- 2 7))))

;; Exercise 1.3
(defn ex3 [a b c]
  (letfn [(square [n] (* n n))]
    (- (apply + (map square (vector a b c)))
       (square (min a b c)))))
(ex3 1 2 3)
;; 13 

;; Exercise 1.4
;; Note that expression can be evaluated to another
;; procedure
(if (> 1 0) - +)
; #function[clojure.core/-]
(defn a-plus-abs-b 
  [a b]
  ((if (> b 0) + -) a b))

;; Exercise 1.5
(defn p []
  (p))
(defn test
  [x y]
  (if (= x 0) 0 y))

(test 0 (p))
;;(err) Execution error (StackOverflowError) 
;; Means Clojure is using applicative-order evaluation

