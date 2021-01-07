(ns sicp_clojure.ch1_text
  (:require [sicp_clojure.common :refer :all]))

(defn abs [x] (if (<= x 0) (- x) x))
;; 1.1.7 Example: Square Roots by Newton’s Method
(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn average [x y] (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (abs (- (* guess guess) x))
     0.001))
     
(defn sqrt [x] (sqrt-iter 1.0 x))

(sqrt 9) 
;; 3.00009155413138 
(sqrt (+ 100 37))
;; 11.704699917758145

;; Exercise 1.6 
(defn new-if 
  [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))    

(new-if (= 2 3) 0 5)
  
(defn sqrt-iter-2
  [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter-2 (improve guess x) x)))

;; (sqrt-iter-2 1.0 2.0)
; eval (root-form): (sqrt-iter-2 1.0 2.0)
; (err) Execution error (StackOverflowError) at sicp-clojure.ch1-text/good-enough?  (REPL:18).

; Exercise 1.7 
(sqrt 0.00000001)
; 0.03125010656242753
(* (sqrt 0.00000001) (sqrt 0.00000001))
; 9.76569160163076E-4
; We can tell that with small number, we got bad precision.
(sqrt 1110000009.001)
; 33316.66263298434
(* (sqrt 1110000009.001) (sqrt 1110000009.001))
; 1.1100000090000951E9

; Exercise 1.8 
(defn cbrt [x]
  (letfn [(cbrt-iter [guess x]
            (letfn [(cube [x] (Math/pow x 3))
                    (good-enough? [guess x]
                      (< (Math/abs (- (cube guess) x)) 0.001))
                    (improve [guess x]
                      (/ (+ (/ x (* guess guess))
                            (* 2 guess))
                         3))])
            (if (good-enough? guess x)
              guess
              (cbrt-iter (improve guess x) x)))]
    (cbrt-iter 1.0 x)))
