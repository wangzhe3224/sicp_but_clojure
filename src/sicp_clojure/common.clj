(ns sicp_clojure.common)

(def line-length 72)
(def nl "\n")
(def dsh-line (reduce str (repeat line-length "-")))
(defn out [title]
  (str dsh-line "\nOutput: Exercise " title "\n" dsh-line "\n"))

(defn -start- [ex-number] (print (out ex-number)))
(defn -end- [] (print "\n\n"))

(defn fn-name [f]
  (first (re-find #"(?<=\$)([^@]+)(?=@)" (str f))))

(defn present-one [func & inputs]
  (run! println (list (str "Calling: `" (fn-name func) "` function") 
                      (str "With   : " inputs)
                      (str "Got    : " (apply func inputs)))))

(present-one str 1 2)
