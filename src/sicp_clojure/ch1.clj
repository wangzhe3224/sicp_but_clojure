(ns sicp_clojure.ch1
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

; 1.3 Formulating abstractions with higher order functions
(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(defn cube [n] (Math/pow n 3))
(defn sum-cubes [a b]
  (sum cube a inc b))

(sum-cubes 1 10)
; 3025.0

(defn identity [x] x)
(defn sum-integers [a b]
  (sum identity a inc b))

(sum-integers 1 3)
; 6

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
; 3.139592655589783
         
(comment
  " once we have `sum` we can build more
  ")

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
; 0.24998750000000042
(integral cube 0 1 0.001)
; 0.249999875000001
; it is actually 0.25 

; Exercise 1.29
(comment
  "
  Exercise 1.29
  Simpson's Rule for integration.
  Note that in following sum function, 0 and inc works as a counter
  to identify termination but not in the actrual calculation.
  ")

(defn simpson [f a b n]
  (def h (/ (- b a) n))
  (defn y [k] (f (+ a (* k h))))
  (defn term [k]
    (cond (= k 0)   (y 0)
          (= k n)   (y n)
          (even? k) (* 2 (y k))
          :else (* 4 (y k))))
  (* (/ h 3)
     (sum term 0 inc n)))

(simpson cube 0 1 100)
; 0.2499999999999999
(simpson cube 0 1 10)
; 0.24999999999999997

(comment 
  "
  Exercise 1.30 
  A iterative version of sum function.
  ")

(defn sum-iter [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(defn sum-it [term a next b]
  "Clojure way kind of tail recursion"
  (loop [a a 
         acc 0]
    (if (> a b) acc
      (recur (next a) 
             (+ acc (term a))))))

(defn sum-cubes-iter [a b]
  (sum-iter cube a inc b))

(sum-cubes-iter 1 10)
; 3025.0

(comment 
  "
  Exercise 1.31

  Write a `product` function similar to general `sum`
  There are few ways to do it. last two ways are more clojure.
  
  Calculate factorial and pi using the prod precedure
  ")

(defn prod [term a next b]
  (if (> a b)
    1 
    (* (term a)
       (prod term (next a) next b))))

(defn prod-iter [term a next b]
  (defn iter [a result]
    (if (> a b) 
      result
      (iter (next a) 
            (* result (term a)))))
  (iter a 1))

(defn prod-it [term a next b]
  (loop [a   a
         acc 1]
    (if (> a b)
      acc
      (recur (next a)
             (* acc (term a))))))

(defn prod-it-map [term a next b]
  (reduce * 1 
          (map term 
               (take-while #(<= % b) (iterate next a)))))

(defn factorial [n prod-func] 
  "n! = n * (n-1) * ... * 1"
  (prod-func identity 1 inc n))

(factorial 5 prod)
(factorial 5 prod-iter)
(factorial 5 prod-it)
(factorial 5 prod-it-map)
; 120

(defn pi [n] 
  (defn term [k] 
    (cond 
      (= k 1) (/ 2.0 3.0)
      (even? k) (/ (* 2.0 k) (- (* 2 k) 1))
      :else (/ (- (* 2.0 k) 1) (* 2 k))))
  (* 4 (prod-it-map term 1 inc n))) 

(pi 500)
; 3.196637745498125
; hmm, it is not great...

(comment 
  "
  Exercise 1.32 

  The `sum` and `prod` above are both special cases of a more general
  process. 
  ")

(defn accumulate [combiner null-value term a next b]
  (if (> a b) 
    null-value
    ; Note here combiner could be + for sum and * for prod
    (combiner (term a)
              (accumulate null-value (next a) next b))))

(defn accumulate-it [combiner null-value term a next b]
  (defn helper [a result]
    (if (> a b)
      result
      (helper (next a) 
              (combiner (term a) result)))))


(comment 
  "
  Exercise 1.33

  A more general accumulate function with filter
  ")

(defn filtered-accumulate 
  [combiner null-value term a next b pedicate]
  (loop [a a
         acc null-value]
    (if (> a b)
      acc 
      (recur (next a)
             (if (predicate a)
               (combiner acc (term a))
               acc)))))
  
(comment " 1.3.2 Using lambda ")

(defn integral-lambda
  [f a b dx]
  (* (sum f 
          (+ a (/ dx 2.0))
          (fn [x] (+ x 4))
          b)
     dx))

(def a (fn [x] x))
(a 1)

(let [x 1
      y 2
      add +]
  (add x y))

(comment " Exercise 1.34 ")

(defn f [g] (g 2))
(defn square [x] (* x x))

(f square)
; 4
(f (fn [z] (* z (+ z 1))))
; 6
(f f)
; (f f) -> (f 2) -> (2 2) -> 2 is not a function, error.

(comment
  "
  Finding roots

  ")

(defn search [f a b]
  (defn close-enough? [x y]
    (defn abs [x] (if (<= x 0) (- x) x))
    (< (abs (- y x)) 0.0001))
  (let [mid-point (/ (+ a b) 2)]
    (if (close-enough? a b)
      mid-point
      (let [test-value (f mid-point)]
        (cond (< test-value 0) (search f mid-point b)
              (> test-value 0) (search f a mid-point)
              :else mid-point)))))

(defn close-enough? [x y]
  (defn abs [x] (if (<= x 0) (- x) x))
  (< (abs (- y x)) 0.0001))

(close-enough? 1 1.0)
    
(defn half-interval-method
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (neg? a-value) (pos? b-value)) (search f a b)
          (and (pos? a-value) (neg? b-value)) (search f b a)
          :else (throw (Exception. "all pos or all neg."))))) 

(defn sin [x] (Math/sin x))
(half-interval-method sin 2.0 4.0)
; 3.141571044921875

(half-interval-method (fn [x] (- (* x x x) (* 2 x) 3)), 1.0, 2.0)
; 1.893280029296875


(comment 
  "
  Finding fix points of function
  ")

(def tolerance 0.00001)
(defn fixed-point 
  [f guess]
  (defn close-enough? [a b]
    (< (Math/abs (- b a)) tolerance))
  (defn try-it [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-it next))))
  (try-it guess))

(defn cos [x] (Math/cos x))
(fixed-point cos 1.0) 
; 0.7390822985224024 


(comment 
  "
  Exercise 1.35

  Use fix point method to compute golden ratio. x -> 1 + 1/x
  ")
(def golden-ratio 
  (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0))
(println golden-ratio)


(comment 
  "
  Exercise 1.36 

  Modify fixed-point so that it prints the sequence of approximations it generates
  ")
(defn fixed-point-log
  [f guess]
  (defn close-enough? [a b]
    (< (Math/abs (- b a)) tolerance))
  (defn try-it [guess]
    (let [next (f guess)]
      (println "Guess " guess)
      (if (close-enough? guess next)
        next
        (try-it next))))
  (try-it guess))

(fixed-point-log cos 1.0) 
; (out) Guess  1.0
; (out) Guess  0.5403023058681398
; (out) Guess  0.8575532158463934
; (out) Guess  0.6542897904977791
; (out) Guess  0.7934803587425656
; (out) Guess  0.7013687736227565
; (out) Guess  0.7639596829006542
; (out) Guess  0.7221024250267077
; (out) Guess  0.7504177617637605
; (out) Guess  0.7314040424225098
; ...

(comment " 1.3.4 Procedure as Return Values")

(defn average [x y] (/ (+ x y) 2))
(defn average-dump [f] (fn [x] (average x (f x))))

(defn sqrt [x]
  (fixed-point (average-dump (fn [y] (/ x y)))
               1.0))
(sqrt 4)
; 2.0000000001

(def dx 0.000001)
(defn deriv [g]
  "derive is yet another function that make a function to
  a new function"
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

(defn cube [x] (* x x x))
((deriv cube) 5)
; 75.00001501625775

(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
(defn newton-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt-2 [x]
  (newton-method
    (fn [y] (- (square y) x)) 1.0))

(sqrt-2 4)
; 2.0000000000000253

; more abstractions
(defn fixed-point-of-transform 
  [g transform guess]
  fixed-point (transform g) guess)


(comment 
  "
  Exercise 1.40 
  ")

(defn cubic [a b c]
  (fn [x] (+ c 
             (* b x) 
             (* a x x)
             (* x x x))))

((cubic 1 1 1) 1)
; 4

(comment
  "
  Exercise 1.41 
  ")

(defn apply-twice [f]
  (fn [x] (f (f x))))

((apply-twice inc) 0)
; 2

(comment
  "
  Exercise 1.42
  Compose
  ")

(defn compose [f g]
  (fn [x] (f (g x))))

((compose square inc) 6)
; 49

(comment
  "
  Exercise 1.43 
  ")

(defn repeated 
  "repeat f for n times"
  [f n]
  (if (= n 1)
    f 
    (compose f (repeated f (dec n))))) 

((repeated square 2) 5)
; 625

(defn smooth [f]
  (fn [x]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(defn n-fold-smooth [f n]
  (repeated (smooth f) n))

(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (loop [guess guess]
      (if (good-enough? guess) guess 
        (recur (improve guess))))))

(defn sqrt [x]
  ((iterative-improve #(< (Math/abs (- (square %) x)) 0.0001)
                      #(/ (+ % (/ x %)) 2))
   x))

(defn fixed-point [f guess]
  ((iterative-improve #(< (Math/abs (- (f %) %)) 0.0001)
                      #(f %))
   guess))
