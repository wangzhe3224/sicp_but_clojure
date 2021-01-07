(ns sicp-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(+ 3 3)

(defn ex3 [a b c]
  (letfn [(square [n] (* n n))]
    (- (apply + (map square (vector a b c)))
       (square (min a b c)))))

(ex3 1 2 3)
