(first '(a b c))
(next '(a b c))

(defn memq [item x]
  (cond (nil? x) false
        (= item (first x)) x
        :else (memq item (next x))))
(memq 'apple '(apple banana))
(memq 'x '(apple banana))

(def a "b")
(= a 'b)
; false

; Exercise 2.53
(list 'a 'b 'c)
; (a b c)
(list (list 'george))
; ((george))
(first '((x1 x2) (y1 y2)))
; (x1 x2)
(next '((x1 x2) (y1 y2)))
; ((y1 y2))

; Exercise 2.54
(defn equal? [left right]
  (cond (and (list? left) (list? right))
        (and (= (first left) (first right))
             (equal? (next left) (next right)))
        (not (or (list? left) (list? right)))
        (= left right)
        :else false))

(equal? '(this is a list) '(this (is a) list))
(equal? 'a 'b)
(equal? 'a 'a)

; Exercise 2.55
(first ''abscsdf)
; quote
(= (quote a) 'a)
; true.. so 

(comment 
  "
  Example: Symbolic Differentiation
  ")

; A data abstraction wishlist, constructor, selector, and predicates.
; I have a feeling that predicates of the data abstraction is the Type.
(defn variable? [e] (symbol? e))
(defn same-variable? [v1 v2] (and (variable? v1) (variable? v2) (= v1 v2)))
(defn make-sum [a1 a2] (list '+ a1 a2))
(defn make-product [m1 m2] (list '* m1 m2))
(defn sum? [e]     (and (= (first e) '+) (list? e)))
(defn product? [e] (and (= (first e) '*) (list? e)))
(defn addend [e] (second e))
(defn augend [e] (nth e 2))
(defn multiplier [e] (second e))
(defn multiplierand [e] (nth e 2))

(defn deriv [e v]
  (cond (number? e) 0
        (variable? e) (if (same-variable? e v) 1 0)
        (sum? e) (make-sum (deriv (addend e) v)
                           (deriv (augend e) v))
        (product? e) 
        (make-sum 
          (make-product (multiplier e)
                        (deriv (multiplierand e) v))
          (make-product (deriv (multiplier e) v)
                        (multiplierand e)))
        :else (throw (Exception. (str "unknown expression type: DERIV" e)))))

; it works but not simplified..
(deriv '(+ x 3) 'x)
; (+ 1 0)
(deriv '(* x y) 'x)
; (+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(comment
  "
  Since we use data abstraction, to add simpification function, 
  we don't need to change `derive` function, just need to tweak 
  the constructor. 
  ")

(defn =number? [e num] (and (number? e) (= e num)))
;; You man notice that we do repeat the code, it is better to 
;; Write a binary-op function to capture + and *
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))
(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

; try again!
(deriv '(+ x 3) 'x)
; 1  <= it was (+ 1 0)
(deriv '(* x y) 'x)
; y  <= (+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

; Exercise 2.56 
; In order to solve this, we need constructor, selector and predicates
; for exponential component.
(defn pow [b e] (Math/pow b e))
(defn make-exponentiation [base exponent]
  (cond (=number? base 0) 0
        (=number? exponent 0) 1
        ;(=number? exponent 1) base 
        (and (number? base) (number? exponent)) (pow base exponent)
        :else (list '** base exponent)))
(defn base [e] (second e))
(defn exponent [e] (nth e 2))
(defn exponentiation? [e]
  (and (list? e) (= (first e) '**)))

(def x (make-exponentiation 'y 1))

(defn deriv [e v]
  (cond (number? e) 0
        (variable? e) (if (same-variable? e v) 1 0)
        (sum? e) (make-sum (deriv (addend e) v)
                           (deriv (augend e) v))
        (product? e) 
        (make-sum 
          (make-product (multiplier e)
                        (deriv (multiplierand e) v))
          (make-product (deriv (multiplier e) v)
                        (multiplierand e)))
        (exponentiation? e) 
        (make-product (make-product (exponent e)
                                    (make-exponentiation (base e)
                                                         (make-sum (exponent e)
                                                                   (- 1))))
        
                      (deriv (base e) v))
        :else (throw (Exception. (str "unknown expression type: DERIV" e)))))

(deriv '(* (** x y) (+ x 3)) 'x)
;;  (+ (** x y) (* (* y (** x (+ y -1))) (+ x 3)))

; Exercise 2.57
(defmacro def-left-assoc-binary-op [symbol constructor
                                    test left right constructor-exprs]
  `(do
     (defn ~constructor [~left & right#]
       (if (nil? right#) ~left
           (let [~right (apply ~constructor right#)]
             (cond ~@constructor-exprs
                   :else (if (~test ~right)
                           (concat (list '~symbol ~left) (rest ~right))
                           (list '~symbol ~left ~right))))))
     (def ~test #(and (list? %) (= (first %) '~symbol)))
     (def ~left second)
     (defn ~right [[_# _# e# & es#]]
       (if (nil? es#) e#
         (apply ~constructor e# es#)))))

(def-left-assoc-binary-op + make-sum sum? addend augend
  [(=number? addend 0) augend
   (=number? augend 0) addend
   (and (number? addend) (number? augend)) (+ addend augend)])

(def-left-assoc-binary-op * make-product product? multiplier multiplicand
  [(or (=number? multiplier 0)
       (=number? multiplicand 0)) 0
   (=number? multiplier 1) multiplicand
   (=number? multiplicand 1) multiplier
   (and (number? multiplicand) (number? multiplier)) (* multiplicand multiplier)])

; Exercise 2.58
(defn make-sum-2 [s1 s2]
  (list s1 '+ s2))
(defn augend [s]
  (first s))
(defn addend [s]
  (nth s 2))

(def x (make-sum-2 'x 'y))
(augend x)

(comment 
  "
  2.3.3 Reprenting Sets
  
  Why we doing this example in chapter on Symbolic Data?
  Just to show different representation has different performance?
  ")

; Sets as unordered lists 
; This is a inefficient representation, O(n)
(defn element-of-set? [x set] 
  (cond (nil? set) false
        (= x (first set)) true 
        :else (element-of-set? x (next set))))
(defn adjoin-set [x set]
  (if (element-of-set? x set) 
    set 
    (cons x set))) 
(defn intersect-set [s1 s2]
  (cond (or (nil? s1) (nil? s2)) nil
        (element-of-set? (first s1) s2) 
        (cons (first s1) (intersect-set (next s1) s2))
        :else (intersect-set (next s1) s2)))

(defn intersect-set [set1 set2]
  (filter #(element-of-set? % set1) set2))


; Exercise 2.59
(defn element-of-set? [x set]
  (some (partial = x) set))

(defn adjoin-set [x set]
  (if (element-of-set? x set) set
      (cons x set)))

(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set1 set2))

; Exercise 2.60 
(defn element-of-set? [x set]
  (some (partial = x) set))

(defn adjoin-set [x set]
  "if with duplication, we dont need to check if elem in set1"
  (cons x set))

(defn intersection-set [set1 set2]
  (filter #(element-of-set? % set1) set2))

(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set1 set2))

(def s1 (list 1 2 3))
(def s2 (list 3 3 3))

(intersection-set s1 s2)

(comment 
  "
  Ordered representation

  We can speed up intersection-set a lot to O(N)
  ")

; Exercise 2.61 
(defn adjoin-set [x set]
  (let [[prefix suffix] (split-with #(< % x) set)]
    (if (= x (first suffix))
      set
      (concat prefix (list x) suffix))))

; Exercise 2.62 
(defn union-set [[s & ss :as s1] [t & ts :as s2]]
  (cond (nil? s1) s2
        (nil? s2) s1
        (= s t) (cons s (union-set ss ts))
        (> s t) (cons t (union-set s1 ts))
        :else   (cons s (union-set ss s2))))

(comment 
  "
  Sets as Binary Trees

  Search of the tree B-tree becomes O(log(n))
  If the tree is roughly balanced.. the worst case can be log(N).
  ")

(defn entry [tree] (first tree))
(defn left-branch [tree] (nth tree 1))
(defn right-branch [tree] (nth tree 2))
(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set]
  "a search"
  (cond (nil? set) 
        false 
        ((= x (entry set))) 
        true
        ((> x (entry set)))
        (element-of-set? x (right-branch set))
        :else 
        (element-of-set? x (left-branch set))))

(defn adjoin-set [x set]
  (cond (nil? set) 
        (make-tree x nil nil)
        (= x (entry set))
        set 
        (> x (entry set))
        (make-tree (entry set)
                   (left-branch set)
                   (adjoin-set x (right-branch set)))
        (< x (first set))
        (make-tree (entry set)
                   (adjoin-set x (left-branch set))
                   (right-branch set))))

; Exercise 2.63
(defn tree-list-1 [tree]
  (if (nil? tree)
    '()
    (concat (tree-list-1 (left-branch tree))
            (cons (entry tree)
                  (tree-list-1 (right-branch tree))))))

(defn tree-list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (nil? tree)
      result-list 
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(def t (make-tree 2 
                  (make-tree 1 nil nil)
                  (make-tree 3 nil nil)))
(def t2 (make-tree 3
                   (make-tree 1 nil nil)
                   (make-tree 7 
                              (make-tree 5 nil nil)
                              (make-tree 9 
                                         nil
                                         (make-tree 11 nil nil)))))
(def t3 (make-tree 5
                   (make-tree 3 
                              (make-tree 1 nil nil)
                              nil)
                   (make-tree 9
                              (make-tree 7 nil nil)
                              (make-tree 11 nil nil))))
(comment
  "
  t:
          2 
         / \
        1   3

  t2: 
          3
         / \
        1   7
           / \
          5   9
               \
                11
  ")

(println (tree-list-1 t))
(println (tree-list-2 t))
(println (tree-list-1 t2))
(println (tree-list-2 t2))
(println (tree-list-1 t3))
(println (tree-list-2 t3))

(comment
  "
  Above two procedures produce same results.
  But the first method is slower than the second one.
  The concat operation is O(N) and given traverse the tree is still log(n),
  it will be nlog(n). Well the second one, con is operated on 1. So it is log(n)
  ")

; Exercise 2.65 
(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(defn partial-tree [elem n]
  " requires: elem is ordered.
    returns: (a tree, elem not yet in previous tree)"
  (if (= n 0)
    (cons '() elem)
   (let [left-size (quot (- n 1) 2)]
     (let [left-result (partial-tree elem left-size)]
       (let [left-tree (first left-result)
             non-left-elem (next left-result)
             right-size (- n (+ left-size 1))]
         (let [this-entry (first non-left-elem)
               right-result (partial-tree (next non-left-elem) right-size)]
           (let [right-tree (first right-result)
                 remaining-elem (next right-result)]
             (cons (make-tree this-entry
                              left-tree 
                              right-tree)
                   remaining-elem))))))))

(def test '(1 3 5 7 9 11))
(partial-tree test 1)
(list->tree test)

(comment 
  "
  list->tree is O(N).
  partial-tree: 
  The tree is composed by 
        this-entry
         /      \
    left-tree  right-tree
  ")

; Exercise 2.65 
(defn union-set-tree [s1 s2]
  (list->tree (union-set (tree-list-2 s1) (tree-list-2 s2))))
(defn intersect-set-tree [s1 s2]
  (list->tree (intersect-set (tree-list-2 s1) (tree-list-2 s2))))

; Exercise 2.66 
(defn lookup-key [k get-key tree]
  (if (nil? tree)
    nil
    (let [record (entry tree)
          record-key (get-key record)]
      (cond (= k record-key) 
            record
            (> k record-key)
            (lookup-key k get-key (right-branch tree))
            :else 
            (lookup-key k get-key (left-branch tree))))))

(defn lookup [k tree]
  "use first item in the records as key"
  (lookup-key k first tree))


(comment
  "
  2.3.4 Example: Huffman Encoding Trees
  ")

; Represent Huffman trees 
; leaf: constructor, selector, predicate
(defn make-leaf [sym w] (list 'leaf sym w))
(defn leaf? [object] (= (first object) 'leaf))
(defn symbol-leaf [x] (nth x 1))
(defn weight-leaf [x] (nth x 2))
; tree: constructor, selector, predicate
(defn left-branch [tree] (first tree))
(defn right-branch [tree] (nth tree 1))
(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))
(defn weight [tree]
  (if (leaf? tree)
      (weight-leaf tree)
      (nth tree 3)))
(defn make-code-tree [left right]
  (list left 
        right 
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; Algorithms for the tree
(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (println "impossible" bit)))

(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (nil? bits)
      '()
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (next bits) tree)) ;; Found, start new round
          (decode-1 (next bits) next-branch))))) ;; Not yet, going down
  (decode-1 bits tree))

(defn adjoin-set [x set]
  "set: a list of Huffman tree nodes
  it is ordered by weight of the node
  retuns: a Huffman tree
  "
  (cond (nil? set) 
        (list x)
        (< (weight x) (weight (first set)))
        (cons x set)
        :else (cons (first set)
                    (adjoin-set x (next set)))))

(defn make-leaf-set [pairs]
  "pairs: ((A 4) (B 2))"
  (if (nil? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)
                             (nth pair 1))
                  (make-leaf-set (next pairs))))))

(def sample-tree 
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree
                     (make-leaf 'D 1)
                     (make-leaf 'C 1)))))
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(comment 
  " Huffman Tree 
           .
        A      .
             B    .
                 D C
  ")
; A B C D
(decode '(0) sample-tree)
(decode '(1 0) sample-tree)
(decode '(1 1 1) sample-tree)
(decode '(1 1 0) sample-tree)
(decode '(0 1 0) sample-tree)
(decode '(0 1 0 1 1 1) sample-tree)
(decode '(0 1 0 1 1 1 1 1 0) sample-tree)
(decode sample-message sample-tree)
; (A D A B B C A)

; Exercise 2.68
(defn contains [sym tree]
  (defn list-contains [lst]
    (cond (nil? lst) false 
          (= sym (first lst)) true 
          :else (list-contains (next lst))))
  (if (leaf? tree)
    (= sym (symbol-leaf tree))
    (list-contains (nth tree 2))))

(contains 'A sample-tree)
(contains 'B (left-branch sample-tree))
(contains 'B (right-branch sample-tree))
(contains 'E sample-tree)

(defn encode-symbol [sym tree]
  "
  sym: 1 symbol 
  tree: Huffman tree
  retruns: list of 0 or 1
  "
  (defn helper [bits current-branch]
    (if (leaf? current-branch)
      (reverse bits)
      (cond (contains sym (left-branch current-branch))
            (helper (cons '0 bits) (left-branch current-branch))
            (contains sym (right-branch current-branch))
            (helper (cons '1 bits) (right-branch current-branch))
            :else (println "error...."))))
  (helper '() tree))

(encode-symbol 'B sample-tree)
(encode-symbol 'D sample-tree)
(encode-symbol 'E sample-tree)

(defn encode [message tree]
  "
  message: list a char, contains only ABCD
  tree: Huffman tree with ABCD code
  "
  (if (nil? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (next message) tree))))

(def re (decode sample-message sample-tree))

(encode re sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; Exercise 2.69 
(defn successive-merge [pairs]
  (cond (nil? pairs) 
        '()
        (= 1 (count pairs)) 
        (first pairs)
        :else 
        (let [fst (first pairs)
              snd (nth pairs 1)
              new-tree (make-code-tree fst snd)]
          (successive-merge (adjoin-set new-tree (next (next pairs)))))))

(defn generate-huffman-tree [pairs] 
   (successive-merge (make-leaf-set pairs)))

