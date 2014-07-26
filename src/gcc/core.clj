(ns gcc.core
  (:require [clojure.java.io :as io]))

(def atom? number?)
(defn quote? [p] (= 'quote (first p)))

(def lambda-counter (atom 0))

(def primitives {
                 '= "CEQ"
                 '< "CLT"
                 '> "CGT"
                 'cons "CONS"
                 'car "CAR"
                 'cdr "CDR"
                 '- "SUB"
                 'atom? "ATOM"
                 })
(defn primitive-1? [p] (and (list? p) (= 2 (count p)) (some #(= % (first p)) (keys primitives))))
(defn primitive-2? [p] (and (list? p) (= 3 (count p)) (some #(= % (first p)) (keys primitives))))

(defn application? [p env] (and (list? p)))

(defn define? [p]
  (= 'define (first p)))

(defn lambda? [p]
  (and (list? p)
       (= 'lambda (first p))))

(defn tif? [p]
  (= 'tif (first p)))

(defn var-ref? [p env]
  (and (symbol? p) (find env p)))

(defn undefined-var-ref? [p env]
  (and (symbol? p) (not (find env p))))

(defn add-name-to-first-instruction [name instructions]
  (let [[first-instruction names] (nth instructions 0)
        remaining-instructions (rest instructions)]
    `[~[first-instruction (vec (conj names (str name)))] ~@remaining-instructions]))


(defn tp [p env]
  (println "tp p" p "env" env)

  (cond
   (atom? p)
   (do
     (println "chose atom")
     [[(str "LDC " p)]])

   (var-ref? p env)
   (do
     (println "chose var-ref")
     [[(env p)]])

   (undefined-var-ref? p env)
   (do
     (println "🙀 chose undefined var-ref for" p)
     [[(str "LDF @" p)]])

   (primitive-1? p)
   (do
     (println "chose primitive-1")
     (let [command (primitives (nth p 0))
           left (vec (flatten (tp (nth p 1) env)))]
       [left [command]]))

   (primitive-2? p)
   (do
     (println "chose primitive-2")
     (let [command (primitives (nth p 0))
           left  (vec (flatten (tp (nth p 1) env)))
           right (vec (flatten (tp (nth p 2) env)))]
       [left right [command]]))

   (lambda? p)
   (do
     (println "chose lambda")
     (swap! lambda-counter #(+ 1 %))
     (let [args (nth p 1)
           body (nth p 2)
           name (str "$lambda-" @lambda-counter)
           new-env (merge env (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))])) [] args)))
           body-instructions (tp body new-env)]
       (add-name-to-first-instruction name body-instructions)))


   (tif? p)
   (do
     (println "chose tif")
     (let [pred (nth p 1)
           left (nth p 2)
           right (nth p 3)
           pred-instructions  (vec (tp pred env))
           left-instructions  (vec (tp left env))
           right-instructions  (vec (tp right env))
           tail-call-in-left (and                             ;; hacky hacky
                              (not (var-ref? left env))
                              (not (primitive-1? left)))
           true-instructions (if tail-call-in-left
                               (let [[l] (last left-instructions)]
                                 `[~@(conj (pop left-instructions) [(str "T" l)])])
                               `[~@left-instructions ["RTN"]])

           false-instructions (if tail-call-in-left
                                `[~@right-instructions ["RTN"]]
                                (let [[l] (last right-instructions)]
                                  `[ ~@(conj (pop right-instructions) [(str "T" l)])]))
           true-offset 1
           false-offset (+ 1 (count true-instructions))
           ]
       `[~@pred-instructions
         ~[(str "TSEL @" true-offset " @" false-offset)]
         ~@true-instructions
         ~@false-instructions]))

   (define? p)
   (do
     (println "chose define")
     (let [name (nth p 1)
           body (nth p 2)
           id (str "LDF @" name) ; todo: need to distinguish between def and defn?
           new-env (merge env {name id})
           body-instructions (tp body new-env)]
       (add-name-to-first-instruction name body-instructions)))


   (application? p env)
   (do
     (println "chose application")
     (let [fun (nth p 0)
           a (println "got fun" fun)
           fun-instructions (tp fun env)
           args (rest p)
           args-instructions (vec (apply concat (map #(tp % env) args)))
           ap-instruction (str "AP " (count args))
           result `[~@args-instructions ~@fun-instructions ~[ap-instruction]]]
       (println "for p" p)
       (println "produce application" result)
       result))

   true (println "i dunno how to tp" p)
   ))

(defn string->number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

(defn add-lines [p-ast]
  (let [[_ p-ast-with-lines names-lines] (reduce (fn [[l p m] [instr name]]
                                                   (if name
                                                     [(+ 1 l) (conj p [l instr]) (merge m {name l})]
                                                     [(+ 1 l) (conj p [l instr]) m]
                                                     ))
                                                 [0 [] {}]
                                                 p-ast)
        result (map (fn [[l instr]]
                      (let [num-replaced (clojure.string/replace instr #"@(\d+)" (fn [[_ n]] (str (+ l (string->number n)))))
                            names-replaced (clojure.string/replace num-replaced #"@(.+)" (fn [[_ n]] (str (names-lines n))))]
                        [l names-replaced]))
                    p-ast-with-lines)]
    result))


(defn gcc [p]
  (let [ast (tp p nil)
        ast-wl (add-lines ast)
        out (clojure.string/join "\n" (flatten (map (fn [[_ instr]] [instr]) ast-wl)))]
    out))
