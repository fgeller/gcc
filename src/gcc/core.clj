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

(defn defun? [p]
  (and (list? p)
       (= 'defun (first p))))

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


(defn tp [p lambdas env] ; => {:result [[]] :lambdas {:l1 [[]]}}
  (println "tp p" p "lambdas" lambdas "env" env)

  (cond
   (atom? p)
   (do
     (println "chose atom")
     {:result [[(str "LDC " p)]] :lambdas lambdas})

   (var-ref? p env)
   (do
     (println "chose var-ref")
     {:result [[(env p)]] :lambdas lambdas})

   (undefined-var-ref? p env)
   (do
     (println "🙀 chose undefined var-ref for" p)
     {:result [[(str "LDF @" p)]] :lambdas lambdas})

   (primitive-1? p)
   (do
     (println "chose primitive-1")
     (let [command (primitives (nth p 0))
           {lr :result l :lambdas} (tp (nth p 1) lambdas env)
           left (vec (flatten lr))]
       {:result [left [command]] :lambdas l}))

   (primitive-2? p)
   (do
     (println "chose primitive-2")
     (let [command (primitives (nth p 0))
           {left-result :result left-lams :lambdas} (tp (nth p 1) lambdas env)
           left  (vec (flatten left-result))
           {right-result :result right-lams :lambdas} (tp (nth p 2) lambdas env)
           right (vec (flatten right-result))]
       {:result [left right [command]] :lambdas (merge left-lams right-lams)}))


   (lambda? p)
   (do
     (println "chose lambda")
     (swap! lambda-counter #(+ 1 %))
     (let [args (nth p 1)
           body (nth p 2)
           name (str "$lambda-" @lambda-counter)
           new-env (merge env (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))])) [] args)))
           {body-instructions :result body-lams :lambdas} (tp body lambdas new-env)
           lambda-instructions (conj (add-name-to-first-instruction name body-instructions) ["RTN"])
           load-lambda [(str "LDF @" name)]]
       {:result [load-lambda] :lambdas (merge lambdas {name lambda-instructions} body-lams)}))


   (tif? p)
   (do
     (println "chose tif")
     (let [pred (nth p 1)
           left (nth p 2)
           right (nth p 3)
           ;; pred-instructions  (vec (tp pred lambdas env))
           {pred-result :result pred-lams :lambdas} (tp pred lambdas env)
           pred-instructions (vec pred-result)

           {left-result :result left-lams :lambdas} (tp left lambdas env)
           left-instructions  (vec left-result)

           {right-result :result right-lams :lambdas} (tp right lambdas env)
           right-instructions  (vec right-result)

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
       {
        :result `[~@pred-instructions
                  ~[(str "TSEL @" true-offset " @" false-offset)]
                  ~@true-instructions
                  ~@false-instructions]
        :lambdas lambdas
        }
       ))

   (defun? p)
   (do
     (println "chose defun")
     (let [name (nth p 1)
           args (nth p 2)
           body (nth p 3)  ;; just single form bodies
           id (str "LDF @" name) ; todo: need to distinguish between def and defn?
           new-env (merge env {name id} (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))])) [] args)))
           {body-instructions :result body-lams :lambdas} (tp body lambdas new-env)
           defun-instructions (conj (add-name-to-first-instruction name body-instructions) ["RTN"])
           ]
       {:result nil :lambdas (merge lambdas body-lams {(str name) defun-instructions})}
       ))

   (application? p env)
   (do
     (println "chose application")
     (let [fun (nth p 0)
           a (println "got fun" fun)
           {fun-instructions :result fun-lams :lambdas} (tp fun lambdas env)
           args (rest p)
           x (map (fn [a] (tp a lambdas env)) args)
           args-instructions (vec (apply concat (map (fn [{res :result lams :lambdas}] res) x)))
           args-lams (reduce (fn [old {res :result lams :lambdas}] (merge old lams)) {} x)
           ap-instruction (str "AP " (count args))
           result `[~@args-instructions ~@fun-instructions ~[ap-instruction]]]
       (println "for p" p)
       (println "produce application" result)
       result
       {:result result :lambdas (merge lambdas fun-lams args-lams)}))

   true (println "i dunno how to tp" p)
   ))

(defn string->number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

(defn add-lines [lams]
  (let [flattened-lams (vec (apply concat (vals lams)))
        a (println "flattened-lams to:" flattened-lams)
        [_ p-ast-with-lines names-lines] (reduce (fn [[l p m] [instr names]]
                                                   (if names
                                                     [(+ 1 l) (conj p [l instr]) (merge m (into {} (map (fn [n] {n l}) names)))]
                                                     [(+ 1 l) (conj p [l instr]) m]
                                                     ))
                                                 [0 [] {}]
                                                 flattened-lams)
        result (map (fn [[l instr]]
                      (let [num-replaced (clojure.string/replace instr #"@(\d+)" (fn [[_ n]] (str (+ l (string->number n)))))
                            names-replaced (clojure.string/replace num-replaced #"@(.+)" (fn [[_ n]] (str (names-lines n))))]
                        [l names-replaced]))
                    p-ast-with-lines)

        ]
    (vec result)))

(defn flatten-funs [funs]
  funs)

(defn gcc [defuns]
  (let [asts (map #(tp % nil nil) defuns) ;; ({:result nil :lambdas {"a" 23}} {:result nil :lambdas {"a" 23}})
        all (apply merge (map #(:lambdas %) asts))
        ast-wl (add-lines all)
        out (clojure.string/join "\n" (flatten (map (fn [[_ instr]] [instr]) ast-wl)))
        ]
    (println "all:" all)
    (println "ast-wl:" ast-wl)
    out
    ))
