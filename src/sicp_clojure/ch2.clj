(ns sicp_clojure.ch2)

(defn factorial 
  [n]
  (defn fact-iter
     [product counter max-count]
     (if (> counter max-count)
       product
       (fact-iter (* counter product) (+ counter 1) max-count)))
  (fact-iter 1 1 n))

(factorial 6)

; tree recursion
; fibonacci
(defn fib 
  [n]
  (cond (= n 0) 0
        (= n 1) 1 
        :else (+ (fib (- n 1)) 
                 (fib (- n 2)))))

(fib 6)

(defn fib-it
  "a <- a + b; b <- a"
  [n]
  (defn fib-iter 
    [a b count]
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib-it 6)

(defn first-denomination
  [kinds-of-coins]
  (cond 
    (= kinds-of-coins 1) 1
    (= kinds-of-coins 2) 5
    (= kinds-of-coins 3) 10
    (= kinds-of-coins 4) 25
    (= kinds-of-coins 5) 50))

; (first-denomination 1)
; 1
(defn cc [amount kinds-of-coins]
  (cond 
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else (+ (cc amount 
                 (- kinds-of-coins 1))
             (cc (- amount (first-denomination kinds-of-coins)) 
                 kinds-of-coins))))

(cc 100 5)

; Exercise 1.11
(defn f-rec [n]
  (cond (< n 3) n
        :else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3))))))

(defn f-itr [n]
  (defn iter [m f-prev f-pre-prev f-pre-pre-prev]
    (def f-this (+ f-prev (* 2 f-pre-prev) (* 3 f-pre-pre-prev)))
    (if (= m n)
      f-this
      (iter (inc m) f-this f-prev f-pre-prev)))
  (if (< n 3)
    n
    (iter 3 2 1 0)))

(f-itr 4)
(f-rec 4)

; Exercise 1.12 
(defn pascal [row col]
  (cond (= row 0) (if (= col 0) 1 (throw (Exception. "Out of range")))
        (= col 0)   1
        (= row col) 1
        (> row 1) (if (or (< col 0) (> col row)) 
                    (throw (Exception. "Out of range"))
                    (+ (pascal (dec row) (dec col))
                       (pascal (dec row) col)))
        :else (throw (Exception. "Out of range"))))

(for [row (range 0 10)
      col (range 0 (inc row))]
  (pascal row col))
