(comment
  "
  4.1.1 The Core of the Evaluator

  For (eval exp env) Primitives expression
  Special forms, qouted, assignment/define, if, lambda, begin, case
  Combinations,

  For (apply proc args)
  ")

(comment
  "
  Representing Expressions
  ")
; Helpers
(defn tagged-list? [exp tag]
  (if (seq exp)
    (= (first exp) tag)
    false))

; Permitives value 
(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

; Variable
(defn variable? [exp]
  (symbol? exp))

; Quote sytax. (quote <text-of-quotation>)
(defn quoted? [exp] (tagged-list? exp 'quote))
(defn text-of-quotation [exp] (second exp))

; Assignment. (set! <var> <value>)
(defn assignment? [exp] (tagged-list? exp 'set!))
(defn assignment-variable [exp] (nth exp 1))
(defn assignment-value [exp] (nth exp 2))

; Lambda. (lambda <params> <body>)
(defn lambda? [exp] (tagged-list? exp 'lambda))
(defn lambda-parameters [exp] (second exp))
(defn lambda-body [exp] (drop 2 exp))
(defn make-lambda [paramters body]
  (list 'lambda paramters body))

(def a (make-lambda '('a 'b) ()))
(lambda-body a)
(lambda-parameters a)

; Definations. (define <var> <value>) 
; Definations for function. (define (<var> <p1> ... <pn>) <body>) 
(defn definition? [exp] (tagged-list? exp 'define))
(defn definition-variable [exp]
  (let [snd (nth exp 1)]
    (if (symbol? snd) snd (first snd))))
(defn definition-value [exp]
  (if (symbol? (second exp))
    (nth exp 2)
    (make-lambda (next (second exp))
                 (nth exp 2))))

(def d '('define '('a 'b) '(+ 1 1)))
(definition-value d)

; Condition. (if <predicate> <consequent> <alternative>)
(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (nth exp 1))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp] 
  (if (not (nil? (nth exp 3)))
    (nth exp 3)
    'false))
(defn make-if [predicate consequent alternative]
  '('if predicate consequent alternative))

; Begin. 
(defn begin? [exp] (tagged-list? exp 'begin))
(defn begin-actions [exp] (next exp))
(defn last-exp? [sequ] (nil? (next sequ)))
(defn first-exp [sequ] (first sequ))
(defn rest-exps [sequ] (next sequ))
(defn make-begin [sequ] '('begin sequ))
(defn sequence->exp [sequ]
  (cond (nil? sequ) sequ
        (last-exp? sequ) (first-exp sequ)
        :else (make-begin sequ)))

; Application
(defn application? [exp] (list? exp))
(defn operator [exp] (first exp))
(defn operands [exp] (next exp))
(defn no-oprands? [ops] (nil? ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (next ops))

; We can also have derived expression using above expressions
; Cond. 
(defn cond? [exp] (tagged-list? exp 'cond))
(defn cond-clauses [exp] (next exp))
(defn cond-predicate [clause] (first clause))
(defn cond-actions [clause] (second clause))
(defn cond-else-clause? [clause] (= (cond-predicate clause) 'else))
(defn expand-clauses [clauses]
  (if-not (seq clauses)
    'false                  ;no else clause
    (let [f (first clauses)
          r (next clauses)]
      (if (cond-else-clause? f)
        (if-not r
          (sequence->exp (cond-actions f))
          (throw (RuntimeException.
                  (format "else clause is not last: %s" clauses))))
        (make-if (cond-predicate f)
                 (sequence->exp (cond-actions f))
                 (expand-clauses r))))))
(defn cond->if [exp] (expand-clauses (cond-clauses exp)))

(def primitive-procedure? #(tagged-list? % 'primitive))
(def primitive-implementation second)

(defn apply-primitive-procedure [proc args]
  (apply (primitive-implementation proc) args))

(comment
  "
  4.1.3 Evaluator Data Structures

  After defined the syntax of expressions, the evaluator also needs to 
  maintain its onw data structures as part of the execution of the 
  program, such as representations of the procedures, environments.
  ")

(defn make-procedure [parameters body env]
  '('procedure parameters body env))
(defn compound-procedure? [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters [p]
  (second p))
(defn procedure-body [p]
  (nth p 2))
(defn procedure-environment [p]
  (nth p 3))

; Environment as a list of fremes
(defn enclosing-environment [env] (rest env))
(defn first-frame [env] (first env))
(def  the-empty-environment '())
(defn make-frame [variables values]
  (atom (zipmap variables values)))
(defn add-binding-to-frame! [var val frame]
  (swap! frame assoc var val))

(defn lookup-variable-value [variable env]
  (some (comp #(get % variable) deref) env))

(defn extend-environment [variables values base-env]
  (when (= (count variables) (count values))
    (cons (make-frame variables values)
          base-env)))

(defn find-first-frame-containing [var env]
  (some #(when (contains? (deref %) var) %) env))

(defn define-variable! [variable value env]
  (add-binding-to-frame! variable value
                         (or (find-first-frame-containing variable env)
                             (first-frame env))))

(defn set-variable-value! [variable value env]
  (when-let [frame (find-first-frame-containing variable)]
    (add-binding-to-frame! variable value frame)))

;; test
(def t-frame (make-frame '('a 'b) '(1 2)))
(add-binding-to-frame 'c 3 t-frame)

;; eval 
(declare eval)

(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval (first-exp exps) env)
    (do
      (eval (first-exp exps) env)
      (recur (rest-exps exps) env))))

(defn apply [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
        (eval-sequence
         (procedure-body procedure)
         (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)))
        :else (throw (RuntimeException.
                      (format "Unknown procedure type: %s" procedure)))))

(defn form [exp _]
  (cond (or (number? exp)
            (string? exp)) :self-evaluating
            (symbol? exp) :variable
            :else (first exp)))

(defmulti eval form)

(defmethod eval :self-evaluating [exp env]
  exp)

(defmethod eval :variable [exp env]
  (lookup-variable-value exp env))

(defmethod eval 'quote [exp env]
  (text-of-quotation exp))

(defmethod eval 'set! [exp env]
  (eval-assignment exp env))

(defmethod eval 'define [exp env]
  (eval-definition exp env))

(defmethod eval 'if [exp env]
  (eval-if exp env))

(defmethod eval 'lambda [exp env]
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(defmethod eval 'begin [exp env]
  (eval-sequence (begin-actions exp) env))

(defmethod eval 'cond [exp env]
  (eval (cond->if exp) env))

(defmethod eval :default [exp env]
  (apply (eval (operator exp) env)
         (list-of-values (operands exp) env)))

; (defn eval [exp env]
;   (cond (self-evaluating? exp) exp
;         (variable? exp) (lookup-variable-value exp env)
;         (quoted? exp) (text-of-quotation exp)
;         (assignment? exp) (eval-assignment exp env)
;         (definition? exp) (eval-definition exp env)
;         (if? exp) (eval-if exp env)
;         (lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env)
;         (begin? exp)
;         (eval-sequence (begin-actions exp) env)
;         (cond? exp) (eval (cond->if exp) env)
;         (application? exp)
;         (apply' (eval (operator exp) env)
;                (list-of-values (operands exp) env))
;         :else (throw (RuntimeException.
;                       (format "Unknown expression: %s" exp)))))

(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval (first-exp exps) env)
    (do 
      (eval (first-exp exps) env)
      (recur (rest-exps exps) env))))

(defn apply [procedure args]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure args)
        (compound-procedure? procedure)
        (eval-sequence
          (procedure-body procedure)
          (extend-environment 
            (procedure-parameters procedure)
            args
            (procedure-environment procedure)))
        :else (throw (RuntimeException. 
                       (format "Unknown procedure type: %s" procedure)))))

(defn list-of-values [exps env]
  (lazy-seq
    (if (no-oprands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env)))))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (definition-value exp) env)
                       env))

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

(defn eval-if [exp env]
  (if (eval (if-predicate exp) env)
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))


(def primitive-procedures
  `((car ~first)
    (cdr ~rest)
    (cons ~cons)
    (null? ~nil?)
    (+ ~+)  
    (* ~*)))

(def primitive-procedure-names (map first primitive-procedures))
(def primitive-procedure-objects (map second primitive-procedures))

(defn setup-environment []
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment (setup-environment))
(def input-prompt "in > ")
(def output-prompt "out >> ")
(def prompt-for-input #(print (newline) (newline) % (newline)))
(def announce-output #(print (newline) % (newline)))

(defn user-print [object]
  (if (compound-procedure? object)
    (print (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
    (print object)))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)
        output (eval input the-global-environment)]
    (announce-output output-prompt)
    (user-print output)))

; (driver-loop)
(eval '(* 5 5) the-global-environment)
(eval '(define x 3) the-global-environment)
(def ex '(* 5 5))
(operator ex)
(operands ex)
(eval (operator ex) the-global-environment)
(apply '* '(5 5))
