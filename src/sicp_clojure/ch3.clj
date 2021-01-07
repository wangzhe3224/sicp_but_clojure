; before we go on a quick review of Clojure's concurrent model
; Clojure has four primitive to handle mutable variable:
; Ref, Agent, Atom, Var
; Ref
(def all-user (ref {}))
(deref all-user)
@all-user
; Mutate Ref need to be done inside dosync
(dosync 
  (ref-set all-user {}))
; Alert
(defn new-user [id login monthly-budget] 
  {:id id
   :login login
   :monthly-budget monthly-budget 
   :total-expenses 0})

(defn add-new-user [login budget-amount]
  (dosync 
    (let [current-number (count @all-user)
          user (new-user (inc current-number) login budget-amount)]
      (alter all-user assoc login user))))

(add-new-user "zhe" 1000)
(add-new-user "z" 2000)

; Agents, async
(def total-cpu-time (agent 0))
(send total-cpu-time + 700)
(deref total-cpu-time)
(send-off total-cpu-time + 700)  ; for blocking operators
; one can use await to block and wait result comes back from agent
; agent is useful in STM model

; Atom, it is sync and no coordination
(def total-rows (atom 0))
(reset! total-rows 100)
(swap! total-rows + 100)

; Var
(def untrusted (with-meta {:command "delete-table" :subject "users"}
                          {:safe false :io true}))
(def ^:dynamic *h* "localhost")
(def ^:dynamic *mysql-host*)
(defn db-query [db]
  (binding [*mysql-host* db]
    (count *mysql-host*)))
(def mysql-hosts ["test-mysql" "dev-mysql" "staging-mysql"])
(pmap db-query mysql-hosts)

(comment
  "
  ok, let's comeback to SICP world.
  3.1 Assigment and local states
  ")

(def balance (atom 100))
(defn withdraw [amount]
  (if (>= @balance amount)
   (do
      (swap! balance - amount)
      @balance)
   "insufficient funds"))
     
(withdraw 60)
(withdraw 50)

; encapsulate balance as internal states
(defn make-withdraw [balance]
  (let [balance (atom balance)]
    (fn [amount]
      (if (>= @balance amount)
        (do
          (swap! balance - amount)
          @balance)
        "insufficient funds"))))

(def W1 (make-withdraw 200))
(def W2 (make-withdraw 200))
(W1 160)
(W2 20)

; Make a more complex back account object
(defn make-account [balance]
  (let [balance (atom balance)]
    (letfn [(withdraw [amount]
               (if (>= @balance amount)
                  (do (swap! balance - amount)
                      @balance)
                  "Insufficient funds."))
            (deposit [amount]
               (do
                 (swap! balance + amount)
                 @balance))
            (dispatch [m]
              (cond (= m 'withdraw) withdraw 
                    (= m 'deposit) deposit
                    :else (str "Unknonw method." m)))]
      dispatch)))

(def acc (make-account 100))
((acc 'deposit) 100)
((acc 'withdraw) 100)

; Exercise 3.1
(defn accumulator [init]
  (let [_state (atom init)]
    (fn [amt]
      (do (swap! _state + amt)
        @_state))))

(def a (accumulator 0))
(a 10)
(a 10)

; Exercise 3.2
(defn make-monitor [func]
  (let [counter (atom 0)]
    (letfn [(how-many-calls? [] @counter)
            (func-call [& args] 
              (swap! counter inc)
              (apply func args))
            (dispatch [m]
              (cond (= m 'how-many-calls?) @counter
                    :else (func-call m)))]
      dispatch)))

(def s (make-monitor (fn [x] (* x x))))
(s 100)
(s 'how-many-calls?)

; Exercise 3.3 3.4
(defn make-account [balance password]
  (let [balance (atom balance)
        attempts (atom 0)]
    (letfn [(call-the-cops []
              (throw (RuntimeException. "Now you're in trouble...")))
            (withdraw [amount]
              (if (< @balance amount)
                (throw (RuntimeException. "Insufficient funds"))
                (swap! balance #(- % amount))))
            (deposit [amount]
              (swap! balance #(+ % amount)))
            (dispatch [pass m]
              (if (= pass password)
                (do
                  (reset! attempts 0)
                  (cond (= m :withdraw) withdraw
                        (= m :deposit) deposit
                        :else (throw (RuntimeException. "Unknown request"))))
                (do
                  (swap! attempts inc)
                  (if (> @attempts 7) (call-the-cops)
                      (throw (RuntimeException. "Password incorrect."))))))]
      dispatch)))

; Exercise 3.5
(defn monte-carlo [trails experiment]
  (loop [remaining trails
         passed 0]
    (cond (= remaining 0) (/ passed trails)
          (experiment) (recur (dec remaining) (inc passed))
          :else (recur (dec remaining) passed))))

(defn rand-in-range [low high]
  (+ low (* (Math/random) (- high low))))

(defn estimate-integral [P x1 x2 y1 y2 trials]
  (let [area (* (- x2 x1) (- y2 y1))]
    (* area
       (monte-carlo trials #(P (rand-in-range x1 x2)
                               (rand-in-range y1 y2))))))

; two versions of factorial
(defn fact [n]
  (defn iter [prod counter]
    (if (> counter n)
      prod
      (iter (* counter prod) (inc counter))))
  (iter 1 1))

(defn fact-clojure [n]
  (loop [prod 1 
         counter 1]
     (cond (> counter n) prod 
           :else (recur (* prod counter) (inc counter)))))

(defn fact-immperative [n]
  (let [prod (atom 1)
        counter (atom 1)]
    (defn iter []
      (if (> @counter n)
         @prod
        (do (reset! prod (* @counter @prod))
            (reset! counter (inc @counter))
            (iter))))
    (iter)))

(fact 4)
(fact-immperative 4)
(fact-clojure 4)

(comment
  "
  3.2 Environment Model of Evaluation

  When apply a procedure:

  1. A new env is created with bindings of the formal parameters of the application.
  2. If the procedure creates a new procedure, then new procedure will point to parent's env.

  Take away is local env is good seperation.
  ")


(comment
  "
  3.3 Modeling with Mutable Data

  In order to model abstract data with state, we need mutators in addition of 
  constructors and selectors.
  ")

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn set-car! [pair item]
  (let [cur-cdr (first (next pair))]
    '(item cur-cdr)))
(defn set-cdr! [pair item]
  (let [cur-car (first pair)]
    '(cur-car item)))

; test 
(defn make-queue []
  '('() '()))
(defn front-ptr [q]
  (first q))
(defn rear-ptr [q]
  (first (next q)))
(defn empty-queue? [q]
  (nil? (front-ptr q)))
(defn front-queue [q]
  (if (empty-queue? q)
    (println "Empty queue")
    (first (front-ptr queue))))
(defn insert-queue! [q item]
  (let [new-pair '(item '())]))
    
(def q (make-queue))
(rear-ptr q)
  

;; TODO: Simulator of Digital Circuits

(comment 
  "
  3.4 Concurrency
  constrain the interleaving of concurrent processes
  Many mechanisms have been developed for this purpose. 
  In this section, we describe one of them, the serializer.
  ")


(comment 
  "
  3.5 Streams
  Alternative ways to model state.
  Need delay evaluation to support 
  On the other hand, the stream framework raises 
  difficulties of its own, and the question of which 
  modeling technique leads to more modular and more easily maintained systems remains open.
  ")

(defn momo-proc [proc]
  (let [already-run? (atom false)
        result (atom false)]
    (fn []
      (if (not @already-run?)
        (do 
          (reset! result (proc))
          (reset! already-run? true)
          @result)
        @result))))

(defn my-delay [proc]
  (momo-proc (fn [] (proc))))
(defn my-force [delayed]
  (delayed))

(def f (fn [] 1))
(def fm (momo-proc f))
(def tmomo (my-delay (fn [] 20)))
(tmomo)
(my-force tmomo)

(defn cons-stream [a b]
  (list a (my-delay b)))
(defn stream-car [s]
  (first s))
(defn stream-cdr [s]
  (my-force (first (next s))))
(defn stream-null? [s]
  (nil? s))

(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(defn stream-for-each [proc s]
  (if (stream-null? s)
    'done 
    (do (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(defn display-stream [s]
  (stream-for-each println s))

(defn stream-map [proc & argstreams]
  (lazy-seq
   (if (empty? (first argstreams)) '()
       (cons
        (apply proc (map first argstreams))
        (apply stream-map (cons proc (map rest argstreams)))))))

(defn stream-enumerate-interval [low high]
  (if (< high low)
    '()
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(def s (stream-enumerate-interval 2 10))
(stream-car s)
(def t (first (next s)))
(t)
(stream-cdr s)
(display-stream (stream-enumerate-interval 1 10))
