;; Register Machine Simulator

;; Register type
(defn make-register [name]
  (let [content (atom 'unasigned)]
    (defn dispatch [message]
      (cond (= message 'get) @content
            (= message 'set) (fn [value] (reset! content value))
            :else (throw (Exception. "Unknown method "))))
    dispatch))

(defn get-content [reg] (reg 'get))
(defn set-content! [reg value] ((reg 'set) value))

; test
(def re (make-register 'test))
(set-content! re 1)
(get-content re)

;; Stack type 
(defn make-stack []
  (let [s (atom '())] 
    (defn push [x] (reset! s (cons x @s)))
    (defn pop_ [] 
      (if (nil? s) (throw (Exception. "Empty Stack: POP"))
        (let [top (first @s)]
          (reset! s (next @s))
          top)))
    (defn initialize []
      (reset! s '())
      :done)
    (defn dispatch [message]
      (cond (= message 'push) push
            (= message 'pop) (pop_)
            (= message 'initialize) (initialize)))
    dispatch))

(defn pops [stack] (stack 'pop))
(defn pushs [stack value] ((stack 'push) value))

; test
(def stack (make-stack))
(pushs stack 1)
(pops stack)

; Make new machine 
(defn make-new-machine []
  (let [pc (make-register 'pc)
        flag (make-register 'flag)
        stack (make-stack)
        the-instruction-sequence '()]
    (letfn [(the-ops [] 
              (list (list 'initialize-stack (fn [] stack 'initialize))))
            (register-table []
              (list (list 'pc pc) (list 'flag flag)))]
      (defn allocate-register [name]))))
    

;; Make a register machine
(defn make-machine [register-names ops controller-text]
  (let [machine (make-new-machine)]
    (do 
      (map #((machine 'allocate-register) %) register-names)
      ((machine 'install-operations) ops)
      ((machine 'install-instruction-seq)
       (assemble controller-text machine)))
    machine))


