(ns sicp_clojure.ch2
  (:require [sicp_clojure.common :refer :all]))

(comment
  "
  # Chapter 2 Building Abstractions with Data

  Building abstractions by combining data objects to form compound data.
  Here are some techneques we are going to use:

  - the notion of closure ( NOT clojure :)
  - interfaces
  - symbolic expression
  - generic operations with data-directed programming
  ")

(comment
  "
  ## 2.1 Introduction to Data Abstractions

  The basic idea of data abstraction is to structure the programs that 
  are use compound data objects so that they operate on the interfaces.
  In general, these interfaces includs: constructor, selector, and others.

  In other words, we want to describe what we can do with this data instead
  determin how to do things.
  ")

; make the Clojure version of car and cdr in scheme
(defn cons [x y]
  (fn [m] (m x y)))
(defn car [lst]
  (lst (fn [x y] x)))
(defn cdr [lst]
  (lst (fn [x y] y)))

(def x (cons 1 2))
(car x)
(cdr x)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))
(defn gcd [a b]
  (if (= b 0)
    a 
    (recur b (mod a b))))

; Define Rational number
; (defn make-rat [n d] (cons n d))
(defn make-rat [n d]
  (let [g (Math/abs (gcd n d))
        m (if (neg? d) (* -1 g) g)]
    (cons (/ n m) (/ d m))))
(defn numer [x] (car x))
(defn denom [x] (cdr x))
(defn print-rat [x]
  (print (numer x))
  (print "/")
  (println (denom x))) 

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))
(print-rat one-half)

; Here we define some operators on rational numbers
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))) 

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y)) 
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; do some tests
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(comment 
  "
  Exercise 2.1
  ")

(defn make-rat [n d]
  (let [g (Math/abs (gcd n d))
        m (if (neg? d) (* -1 g) g)]
    (cons (/ n m) (/ d m))))

(comment 
  "
  2.1.2 Abstraction Barriers

  The key idea is to seperate interfaces and implementation.
  And using layers.
  ")

(comment
  "
  Exercise 2.2 
  ")

(defn make-segment 
  [start end]
  [start, end])
(def start-segment first)
(def end-segment second)

(defn make-point [x y] [x, y])
(def x-point first)
(def y-point second)

(defn midpoint-segment [segment]
  (make-point
   (/ (+ (x-point (start-segment segment))
         (x-point (end-segment segment)))
      2)
   (/ (+ (y-point (start-segment segment))
         (y-point (end-segment segment)))
      2)))

(defn print-point [p]
  (println (format "(%d, %d)"
                   (x-point p)
                   (y-point p))))

(comment 
  "
  2.1.3 What is Meant by Data?

  Data is defined by collection of selectors and constructors,
  together with specified conditions that these procedures must
  fulfill. 
  ")

(defn cons [x y]
  "This demonstrates that the ability to manipulate 
  procedures as objects automatically provides the ability to 
  represent compound data."
  (defn dispatch [m]
    (cond ((= m 0) x)
          ((= m 1) y)
          :else (throw "Argument not 0 or 1: CONS")))
  dispatch)
(defn car [z] (z 0))
(defn cdr [z] (z 1))

(comment
  "
  Exercise 2.4
  ")

(defn cons [x y]
  (fn [m] (m x y)))
(defn car [lst]
  (lst (fn [x y] x)))
(defn cdr [lst]
  (lst (fn [x y] y)))

(comment
  "
  Exercise 2.5
  ")
(defn cons [x y]
  (* (Math/pow 2 x)
     (Math/pow 3 y)))

(defn divides-count [n d]
  (loop [n (int n)
         cnt 0]
    (if (not= (mod n d) 0) cnt
        (recur (/ n d) (inc cnt)))))

(defn car [pair]
  (divides-count pair 2))

(defn cdr [pair]
  (divides-count pair 3))

(comment
  "
  Exercise 2.6
  ")

(def zero 
  (fn [f] (fn [x] x)))
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))
(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

(defn add [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))

(comment 
  "
  2.1.4 Extended Exercise: Interval Arithmetic

  Exercise 2.7
  ")

(defn make-interval [x y] [x, y])
(def lower-bound first)
(def upper-bound second)
(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(defn div-interval [x y]
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))
;;; Exercise 2.8
(defn sub-interval [a b]
  (make-interval (max (- (upper-bound a)
                         (lower-bound b))
                      (- (upper-bound b)
                         (lower-bound a)))
                 (min (- (lower-bound a)
                         (upper-bound b))
                      (- (lower-bound b)
                         (upper-bound a)))))
;;; Exercise 2.12

(defn make-center-percent [c p]
  (let [ratio (/ p 100.0)]
    (make-interval (+ c (* c ratio))
                   (- c (* c ratio)))))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn percent [i]
  (let [width (- (upper-bound i) (center i))]
    (* 100 (/ width (center i)))))

;;; Exercise 2.15 R1R2 could be come very large..

; Exercise 2.17
(defn last-pair [lst] (last lst))
(last-pair [1 2 3])

; Exercise 2.18
(defn rev [lst]
   (loop [c lst 
          acc '()]
     (if c (recur (next c)
                  (conj acc (first c)))  
         acc)))
(rev [1, 2, 3, [1,2]])

; Exercise 2.19 
(def us-coins [50, 25, 10, 5, 1])
(def uk-coins [100, 50, 20, 10, 5, 2, 1, 0.5])
(def no-more? nil?)
(def except-first-denomination next)
(def first-denomination first)
(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount 
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))

(cc 100 us-coins)
; 292

; Exercise 2.20 
(defn same-parity [x & xs]
  cons x (filter (if (even? x) even? odd?) xs))
(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6)

; Exercise 2.21
(defn square-list [lst] (map (fn [x] (* x x)) lst))
(square-list (list 1 2 3))

; Exercise 2.23
(defn for-each-lazy [f coll]
  (map f coll))
(defn for-each-strict [f coll]
  (doall (for-each-lazy f coll)))

(for-each-strict (fn [x] (println x)) '(1 2 3))

(comment
  "
  2.2.2 Hierarchical Sturctures
  Map, fold, filter still powerful in nested structures, like trees and graph.
  ")

(defn count-leaves [x]
  (cond (nil? x) 0 
        (not (list? x)) 1))

(count-leaves '()) 

; Exercise 2.33 
(defn map' [f xs]
  (reduce #(concat %1 (list (f %2))) '() xs))
(defn append' [coll1 coll2]
  (reduce conj coll2 (reverse coll1))) 
(defn length' [coll]
  (reduce (fn [acc x] (+ 1 acc)) 0 coll))

(map' #(* 2 %1) '(1 2 3))
(append' '(1 2 3) '(1 2 3))
(length' '(1 2))

; Exercise 2.34
(defn foldr [func init coll]
  (if (nil? coll) init 
    (func (first coll)
          (foldr func init (next coll)))))

(defn horner-eval [x coefficient-sequence]
  (foldr #(+ (* %2 x) %1) 0 coefficient-sequence))

(horner-eval 2 '(1 3 0 5 0 1))

; Exercise 2.36 
(defn accumulate [op init coll]
  (if (nil? coll) init
    ((println coll)
     (op (first coll)
         (accumulate op init (next coll))))))
(defn accumulate-n [op init coll]
  (if (nil? (first coll))
    nil 
    (cons (accumulate op init (map first coll))
          (accumulate-n op init (map next coll)))))

(def s1 '( '(1 2 3) '(4 5 6)))

; Exercise 2.37 
(defn dot-product [v w]
  (reduce + 0 (map * v w)))
(defn matrix-*-vector [m v]
  (map (partial dot-product v) m))
(defn transpose [mat] 
  (accumulate-n cons nil mat))
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row] (map #(dot-product row %) cols)) m)))
  
(comment 
  "
  Nested Mappings
  ")

; Exercise 2.40 
(defn unique-pairs [n]
  (mapcat (fn [x]
            (map (partial list x) (range (inc x) (inc n))))
          (range 1 n)))

(unique-pairs 5)
; ((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5))

; Exercise 2.41 
(defn triples [n]
  (for [x (range 1 (dec n))
        y (range (inc x) n)
        z (range (inc y) (inc n))]
    [x y z]))

; Exercise 2.42 Eight-queens
(def empty-board '())
(defn safe? [positions]
  (let [[queen-pos & left] positions
        k (count positions)
        diags-up (map - left (range 1 (inc k)))
        diags-down (map + left (range 1 (inc k)))]
    (empty? (filter (partial = queen-pos) (concat diags-up diags-down left)))))
(defn queens [board-size]
  ((fn queen-cols [k]
     (if (zero? k)
       (list empty-board)
       (for [rest-of-queens (queen-cols (dec k))
             new-row (range 1 (inc board-size))
             :let [new-cols (cons new-row rest-of-queens)]
             :when (safe? new-cols)]
         new-cols)))
   board-size))

(queens 4)

(comment 
  "
  Example: A Picture Language
  ")


