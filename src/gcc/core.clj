(ns gcc.core
  (:require [clojure.java.io :as io])
  (:gen-class :main true))

;; DONE nil
;; DONE TAP / RTN fix
;; DONE env passing for lambdas
;; TODO automatic main insertion?

(defn string->number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

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
                 '+ "ADD"
                 '- "SUB"
                 '* "MUL"
                 '/ "DIV"
                 'atom? "ATOM"
                 'dbg "DBUG"
                 })

(defn primitive-1? [p] (and (list? p) (= 2 (count p)) (some #(= % (first p)) (keys primitives))))
(defn primitive-2? [p] (and (list? p) (= 3 (count p)) (some #(= % (first p)) (keys primitives))))

(defn application? [p env] (and (list? p)))

(defn built-in-mktuple [p lambdas env eval]
  (let [args (rest p)
        evaluated-args (map (fn [a] (eval a lambdas env)) args)
        args-instructions (vec (apply concat (map (fn [{res :result lams :lambdas}] res) evaluated-args)))
        args-lams (reduce (fn [old {res :result lams :lambdas}] (merge old lams)) {} evaluated-args)
        cons-chain (vec (map (fn [_] ["CONS"]) (pop args)))
        list-instructions (vec (concat args-instructions cons-chain))]
    {:result list-instructions :lambdas (merge lambdas args-lams)}))

(defn built-in-mklist [p lambdas env eval]
  (let [args (rest p)
        evaluated-args (map (fn [a] (eval a lambdas env)) args)
        args-instructions (vec (apply concat (map (fn [{res :result lams :lambdas}] res) evaluated-args)))
        args-lams (reduce (fn [old {res :result lams :lambdas}] (merge old lams)) {} evaluated-args)
        cons-chain (vec (map (fn [_] ["CONS"]) args))
        list-instructions (vec (concat (conj args-instructions ["LDC 0"]) cons-chain))]
       {:result list-instructions :lambdas (merge lambdas args-lams)}))

(def built-in-functions {
                         'mklist built-in-mklist
                         'mktuple built-in-mktuple
                         })
(defn built-in-function? [p] (and (list? p) (find built-in-functions (first p))))

(defn defun? [p]
  (and (list? p)
       (= 'defun (first p))))

(defn lambda? [p]
  (and (list? p)
       (= 'lambda (first p))))

(defn tif? [p]
  (and (list? p)
       (= 'tif (first p))))

(defn if? [p]
  (and  (list? p)
        (= 'if (first p))))

(defn var-ref? [p env]
  (and (symbol? p) (find env p)))

(defn undefined-var-ref? [p env]
  (and (symbol? p) (not (find env p))))

(defn add-name-to-first-instruction [name instructions]
  (let [[first-instruction names] (nth instructions 0)
        remaining-instructions (rest instructions)]
    `[~[first-instruction (vec (conj names (str name)))] ~@remaining-instructions]))

(defn maybe-add-rtn [instructions]
  (let [[last-instruction _] (last instructions)
        is-tap (re-matches #"TAP \d+" last-instruction)]
    (if is-tap instructions
        (conj instructions ["RTN"]))))

(defn tp [p lambdas env] ; => {:result [[]] :lambdas {:l1 [[]]}}
  (println (format "tp p[%s] lambdas[%s] env[%s]" p lambdas env))

  (cond
   (atom? p)
   (do
     (println "chose atom")
     {:result [[(str "LDC " p)]] :lambdas lambdas})

   (var-ref? p env)
   (do
     (println "chose var-ref")
     {:result [[(env p)]] :lambdas lambdas})

   (or (nil? p) (false? p))
   (do
     (println "chose nil/false")
     {:result [["LDC 0"]] :lambdas lambdas})

   (true? p)
   (do
     (println "chose true")
     {:result [["LDC 1"]] :lambdas lambdas})

   (undefined-var-ref? p env)
   (do
     (println "🙀  chose undefined var-ref for" p)
     {:result [[(str "LDF @" p)]] :lambdas lambdas})

   (primitive-1? p)
   (do
     (println "chose primitive-1")
     (let [command (primitives (nth p 0))
           {left-result :result lams :lambdas} (tp (nth p 1) lambdas env)]
       {:result `[~@left-result ~[command]] :lambdas (merge lams)}))

   (primitive-2? p)
   (do
     (println "chose primitive-2")
     (let [command (primitives (nth p 0))
           {left-result :result left-lams :lambdas} (tp (nth p 1) lambdas env)
           {right-result :result right-lams :lambdas} (tp (nth p 2) lambdas env)]
       {:result `[~@left-result ~@right-result ~[command]] :lambdas (merge left-lams right-lams)}))


   (lambda? p)
   (do
     (println "chose lambda")
     (swap! lambda-counter #(+ 1 %))
     (let [args (nth p 1)
           body (nth p 2)
           name (str "$lambda-" @lambda-counter)
           env-with-updated-arg-refs (into {} (map (fn [[name instr]]
                                                     [name (clojure.string/replace instr #"LD (\d+) (\d+)" (fn [[_ frm arg]] (str "LD " (+ 1 (string->number frm)) " " arg)))])
                                                   env))
           new-env (merge env-with-updated-arg-refs (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))])) [] args)))
           {body-instructions :result body-lams :lambdas} (tp body lambdas new-env)
           lambda-instructions (conj (add-name-to-first-instruction name body-instructions) ["RTN"])
           load-lambda [(str "LDF @" name)]]
       {:result [load-lambda] :lambdas (merge lambdas {name lambda-instructions} body-lams)}))

   (if? p)
   (do
     (println "chose if")
     (let [pred (nth p 1)
           left (nth p 2)
           right (nth p 3)
           {pred-result :result pred-lams :lambdas} (tp pred lambdas env)
           pred-instructions (vec pred-result)

           {left-result :result left-lams :lambdas} (tp left lambdas env)
           left-instructions  (vec left-result)

           {right-result :result right-lams :lambdas} (tp right lambdas env)
           right-instructions  (vec right-result)

           true-instructions `[~@left-instructions ["RTN"]]
           false-instructions `[~@right-instructions ["RTN"]]

           true-offset 1
           false-offset (+ 1 (count true-instructions))
           ]
       {
        :result `[~@pred-instructions
                  ~[(str "TSEL @" true-offset " @" false-offset)]
                  ~@true-instructions
                  ~@false-instructions]
        :lambdas (merge lambdas pred-lams left-lams right-lams)
        }
       ))
   ;; (if? p)
   ;; (do
   ;;   (println "chose if")
   ;;   (let []
   ;;     {:result [] :lambdas lambdas}))

   (tif? p)
   (do
     (println "chose tif")
     (let [pred (nth p 1)
           left (nth p 2)
           right (nth p 3)
           {pred-result :result pred-lams :lambdas} (tp pred lambdas env)
           pred-instructions (vec pred-result)

           {left-result :result left-lams :lambdas} (tp left lambdas env)
           left-instructions  (vec left-result)

           {right-result :result right-lams :lambdas} (tp right lambdas env)
           right-instructions  (vec right-result)

           tail-call-in-left (and                             ;; hacky hacky
                              (not (atom? left))
                              (not (nil? left))
                              (not (true? left))
                              (not (var-ref? left env))
                              (not (undefined-var-ref? p env))
                              (not (lambda? p))
                              (not (if? p))
                              (not (tif? p))
                              (not (primitive-1? left))
                              (not (primitive-2? left)))
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
        :lambdas (merge lambdas pred-lams left-lams right-lams)
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
           defun-instructions (maybe-add-rtn (add-name-to-first-instruction name body-instructions))
           ]
       {:result nil :lambdas (merge lambdas body-lams {(str name) defun-instructions})}
       ))

   (built-in-function? p)
   (do
     (println "chose built-in")
     ((built-in-functions (first p)) p lambdas env tp))

   (application? p env)
   (do
     (println "chose application")
     (let [fun (nth p 0)
           {fun-instructions :result fun-lams :lambdas} (tp fun lambdas env)
           args (rest p)
           x (map (fn [a] (tp a lambdas env)) args)
           args-instructions (vec (apply concat (map (fn [{res :result lams :lambdas}] res) x)))
           args-lams (reduce (fn [old {res :result lams :lambdas}] (merge old lams)) {} x)
           ap-instruction (str "AP " (count args))
           result `[~@args-instructions ~@fun-instructions ~[ap-instruction]]]
       ;; (println "for p" p)
       ;; (println "produce application" result)
       result
       {:result result :lambdas (merge lambdas fun-lams args-lams)}))

   true (println "i dunno how to tp" p)
   ))

(defn add-lines [lams]
  (let [main-fun (lams "main")
        ordered-flattened-lams (vec (apply concat (cons main-fun (vals (dissoc lams "main")))))
        [_ p-ast-with-lines names-lines] (reduce (fn [[l p m] [instr names]]
                                                   (if names
                                                     [(+ 1 l) (conj p [l (str instr " ; " (clojure.string/join ", " names))]) (merge m (into {} (map (fn [n] {n l}) names)))]
                                                     [(+ 1 l) (conj p [l instr]) m]
                                                     ))
                                                 [0 [] {}]
                                                 ordered-flattened-lams)
        result (map (fn [[l instr]]
                      (let [num-replaced (clojure.string/replace instr #"@(\d+)" (fn [[_ n]] (str (+ l (string->number n)))))
                            names-replaced (clojure.string/replace num-replaced #"@([^ ]+)" (fn [[_ n]]
                                                                                           (when-not (names-lines n) (throw (Exception. (str "Unknown name: [" n "]"))))
                                                                                           (str (names-lines n))))]
                        [l names-replaced]))
                    p-ast-with-lines)

        ]
    (vec result)))

(defn gcc [defuns]
  (let [base-env {}
        base-lambdas {}
        asts (map #(tp % base-lambdas base-env) defuns)
        all (apply merge (map #(:lambdas %) asts))
        ast-wl (add-lines all)
        out (clojure.string/join "\n" (flatten (map (fn [[_ instr]] [instr]) ast-wl)))]
    out
    ))

(defn -main [& args]
  (let [in  (apply concat (map (fn [f] (read-string (slurp f))) args))
        out (gcc in)
        out-file (spit "lambdaman.gcc" out)]
    (println (format "input files:\n====================\n%s\n====================" (clojure.string/join "\n--------------------\n" in)))
    (println (format "output file:\n====================\n%s\n====================" out))
    (println "wrote output to lambdaman.gcc")
    (println "good luck. 🙋")))
