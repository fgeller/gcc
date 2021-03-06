(ns gcc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:gen-class :main true))

(declare built-in-mklist)
(declare built-in-mktuple)
(declare compile-sexprs-to-gcc)

(def lambda-counter (atom 0))
(def branch-counter (atom 0))


(def primitive-values {
                      nil "LDC 0"
                      false "LDC 0"
                      true "LDC 1"
                      })
(def built-in-functions {
                         'mklist built-in-mklist
                         'mktuple built-in-mktuple
                         })

(def primitives {
                 '= {:compile "CEQ"}
                 '> {:compile "CGT"}
                 '>= {:compile "CGTE"}
                 'cons {:compile "CONS"}
                 'car {:compile "CAR"}
                 'cdr {:compile "CDR"}
                 '+ {:compile "ADD" :interpret +}
                 '- {:compile "SUB" :interpret -}
                 '* {:compile "MUL"}
                 '/ {:compile "DIV"}
                 'atom? {:compile "ATOM"}
                 'dbg {:compile "DBUG"}
                 'brk {:compile "BRK"}
                 })

(defn string->number [str]
  (let [n (read-string str)]
    (when (number? n) n)))

(def atom? number?)

(defn primitive? [p]
  (and
   (seq? p)
   (some #(= % (count p)) '(1 2 3))
   (some #(= % (first p)) (keys primitives))))

(defn primitive-value? [p]
  (or (nil? p)
      (true? p)
      (false? p)))

(defn application? [p] (seq? p))

(defn built-in-function? [p] (and (seq? p) (find built-in-functions (first p))))

(defn quoted? [p] (and (seq? p) (= 'quote (first p))))

(defn defun? [p]
  (and (seq? p)
       (= 'defun (first p))))

(defn lambda? [p]
  (and (seq? p)
       (= 'lambda (first p))))

(defn if? [p]
  (and  (seq? p)
        (= 'if (first p))))

(defn var-ref? [p env]
  (and (symbol? p) (find env p)))

(defn let? [p]
  (and (seq? p)
       (= 'let (first p))))

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

(defn has-tail-call [instructions env]
  (and
   (< 1 (count instructions))
   (re-matches #"AP \d+" (first (last instructions)))
   (.endsWith (first (nth instructions (- (count instructions) 2)))
              (str (:current-fun env)))))

(defn append-branches [instructions branches]
  (concat instructions (vec (apply concat (vals branches)))))

(defn interpret-sexp [sexp]
  (cond
   (atom? sexp) sexp

   (primitive? sexp)
   (let [args (map interpret-sexp (rest sexp))]
     (apply (:interpret (primitives (first sexp))) args))))

(defn rewrite-sexp [ast]
  (cond
   (let? ast)
   (let [bindings (nth ast 1)
         body (nthrest ast 2)
         reduced-lambdas (first (reduce (fn [last next]
                                          (let [name (first next)
                                                value-body (second next)
                                                next-body `(((~'lambda (~name) ~@last) ~value-body))]
                                            next-body))
                                        body
                                        (reverse bindings)))
         single-lambda `((~'lambda () ~@body))
         rewritten-lambdas (if (= 0 (count bindings)) single-lambda reduced-lambdas)]
     rewritten-lambdas)

   (quoted? ast)
   (let [rewrite-body (defn rw [a] (if (seq? a) (cons 'mklist (map rw a)) a))
         rewritten (first (map rw (rest ast)))]
     rewritten)

   ;; TODO: need this one?
   (primitive? ast) (cond (= 1 (count ast)) ast
                          (= 2 (count ast)) `(~(first ast) ~(rewrite-sexp (second ast)))
                          (= 3 (count ast)) `(~(first ast) ~(rewrite-sexp (second ast)) ~(rewrite-sexp (nth ast 2))))

   (lambda? ast)            `(~(nth ast 0) ~(nth ast 1) ~@(map rewrite-sexp (nthrest ast 2)))
   (defun? ast)             `(~(nth ast 0) ~(nth ast 1) ~(nth ast 2) ~@(map rewrite-sexp (nthrest ast 3)))
   (if? ast)                `(~(nth ast 0) ~(rewrite-sexp (nth ast 1)) ~(rewrite-sexp (nth ast 2)) ~(rewrite-sexp (nth ast 3)))
   (built-in-function? ast) `(~(nth ast 0) ~@(map rewrite-sexp (nthrest ast 1)))
   (application? ast)       `(~(rewrite-sexp (nth ast 0)) ~@(map rewrite-sexp (nthrest ast 1)))
   true ast))

(defn compile-sexp-to-gcc [p lambdas env]
  ;; (println (format "compile-sexp-to-gcc p[%s] lambdas[%s] env[%s]" p lambdas env))

  (cond
   (atom? p)
   (do
     ;; (println "chose atom")
     {:result [[(str "LDC " p)]] :lambdas lambdas :branches {}})

   (var-ref? p env)
   (do
     ;; (println "chose var-ref")
     {:result [[(env p)]] :lambdas lambdas :branches {}})

   (primitive-value? p)
   (do
     ;; (println "chose primitive value")
     {:result [[(primitive-values p)]] :lambdas lambdas :branches {}})

   (undefined-var-ref? p env)
   (do
     ;; (println "🙀  chose undefined var-ref for" p)
     {:result [[(str "LDF @" p)]] :lambdas lambdas :branches {}})

   (primitive? p)
   (do
     ;; (println "chose primitive")
     (cond (= 1 (count p)) (let [command (:compile (primitives (nth p 0)))]
                             {:result `[~[command]] :lambdas lambdas :branches {}})
           (= 2 (count p)) (let [command (:compile (primitives (nth p 0)))
                                 {left-result :result lams :lambdas left-branches :branches} (compile-sexp-to-gcc (nth p 1) lambdas env)]
                             {
                              :result `[~@left-result ~[command]]
                              :lambdas (merge lams)
                              :branches left-branches
                              }
                             )
           (= 3 (count p)) (let [command (:compile (primitives (nth p 0)))
                                 {left-result :result left-lams :lambdas left-branches :branches} (compile-sexp-to-gcc (nth p 1) lambdas env)
                                 {right-result :result right-lams :lambdas right-branches :branches} (compile-sexp-to-gcc (nth p 2) lambdas env)]
                             {
                              :result `[~@left-result ~@right-result ~[command]]
                              :lambdas (merge left-lams right-lams)
                              :branches (merge left-branches right-branches)
                              })))

   (lambda? p)
   (do
     ;; (println "chose lambda")
     (swap! lambda-counter #(+ 1 %))
     (let [args (nth p 1)
           bodyrest (nthrest p 2)

           name (str "$lambda-" @lambda-counter)
           env-with-updated-arg-refs (into {} (map (fn [[name instr]]
                                                     [name (string/replace instr #"LD (\d+) (\d+)" (fn [[_ frm arg]] (str "LD " (+ 1 (string->number frm)) " " arg)))])
                                                   env))
           new-env (merge env-with-updated-arg-refs
                          (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))])) [] args))
                          {:current-fun name})

           [body-instructions body-lams body-branches] (compile-sexprs-to-gcc bodyrest compile-sexp-to-gcc lambdas new-env)

           lambda-instructions (append-branches (conj (add-name-to-first-instruction name body-instructions) ["RTN"])
                                                body-branches)
           load-lambda [(str "LDF @" name)]]
       {
        :result [load-lambda]
        :lambdas (merge lambdas {name lambda-instructions} body-lams)
        :branches {}
        }))

   (if? p)
   (do
     ;; (println "chose if")
     (let [pred (nth p 1)
           left (nth p 2)
           right (nth p 3)
           {pred-result :result pred-lams :lambdas pred-branches :branches} (compile-sexp-to-gcc pred lambdas env)
           pred-instructions (vec pred-result)

           {left-result :result left-lams :lambdas left-branches :branches} (compile-sexp-to-gcc left lambdas env)
           left-instructions  (vec left-result)

           {right-result :result right-lams :lambdas right-branches :branches} (compile-sexp-to-gcc right lambdas env)
           right-instructions  (vec right-result)

           ;; TODO only optimize if last statement
           tail-call-in-left (has-tail-call left-instructions env)
           tail-call-in-right (has-tail-call right-instructions env)
           ]
       (cond (or tail-call-in-left tail-call-in-right)
             (let [true-instructions (if tail-call-in-left
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
                :branches {}
                })

             true ; normal if
             (let [_ (swap! branch-counter #(+ 1 %))
                   left-branch-name (str "$" (:current-fun env) "-left-branch-" @branch-counter)
                   left-branch (conj (add-name-to-first-instruction left-branch-name left-instructions) ["JOIN"])


                   _ (swap! branch-counter #(+ 1 %))
                   right-branch-name (str "$" (:current-fun env) "-right-branch-" @branch-counter)
                   right-branch (conj (add-name-to-first-instruction right-branch-name right-instructions) ["JOIN"])
                   ]
               {
                :result `[~@pred-instructions
                          ~[(str "SEL @" left-branch-name " @" right-branch-name)]]
                :lambdas (merge lambdas pred-lams left-lams right-lams)
                :branches (merge
                           pred-branches
                           left-branches
                           right-branches
                           {left-branch-name left-branch right-branch-name right-branch})
                })
             ))
     )

   (defun? p)
   (do
     ;; (println "chose defun")
     (let [name (nth p 1)
           args (nth p 2)
           bodyrest (nthrest p 3)

           load-instruction (str "LDF @" name)
           new-env (merge env
                          (into {} (reduce (fn [a b] (conj a [b (str "LD 0 " (count a))]))
                                           []
                                           args))
                          {:current-fun name name load-instruction})

           [body-instructions body-lams body-branches] (compile-sexprs-to-gcc bodyrest compile-sexp-to-gcc lambdas new-env)

           defun-instructions (append-branches (maybe-add-rtn (add-name-to-first-instruction name body-instructions))
                                               body-branches)
           ]
       {
        :result nil
        :lambdas (merge lambdas body-lams {(str name) defun-instructions})
        :branches {}
        }
       ))

   (built-in-function? p)
   (do
     ;; (println "chose built-in")
     ((built-in-functions (first p)) p lambdas env compile-sexp-to-gcc))

   (application? p)
   (do
     ;; (println "chose application")
     (let [fun (nth p 0)
           {fun-instructions :result fun-lams :lambdas fun-branches :branches} (compile-sexp-to-gcc fun lambdas env)
           args (rest p)
           [args-instructions args-lams args-branches] (compile-sexprs-to-gcc args compile-sexp-to-gcc lambdas env)
           ap-instruction (str "AP " (count args))
           result `[~@args-instructions ~@fun-instructions ~[ap-instruction]]]
       result
       {
        :result result
        :lambdas (merge lambdas fun-lams args-lams)
        :branches (merge fun-branches args-branches)
        }))

   true (println "i dunno how to compile-sexp-to-gcc" p "with type" (type p))
   ))

(defn compile-sexprs-to-gcc [forms compile lambdas env]
  (let [compiled (map (fn [a] (compile a lambdas env)) forms)
        result (vec (apply concat (map (fn [{res :result}] res) compiled)))
        lams (reduce (fn [old {lams :lambdas}] (merge old lams)) {} compiled)
        branches (reduce (fn [old {branches :branches}] (merge old branches)) {} compiled)]
    [result lams branches]))

(defn built-in-list-tuple [p lambdas env compile is-list]
  (let [args (rest p)
        [args-instructions args-lams args-branches] (compile-sexprs-to-gcc (rest p) compile lambdas env)

        cons-chain (vec (map (fn [_] ["CONS"]) (if is-list args (pop args))))
        list-instructions (vec (concat (if is-list
                                         (conj args-instructions ["LDC 0"])
                                         args-instructions) cons-chain))]
    {
     :result list-instructions
     :lambdas (merge lambdas args-lams)
     :branches args-branches
     }))

(defn built-in-mktuple [p lambdas env compile]
  (built-in-list-tuple p lambdas env compile false))

(defn built-in-mklist [p lambdas env compile]
  (built-in-list-tuple p lambdas env compile true))

(defn add-lines [lams]
  (let [main-fun (lams "main")
        ordered-flattened-lams (vec (apply concat (cons main-fun (vals (dissoc lams "main")))))
        [_ p-ast-with-lines names-lines] (reduce (fn [[l p m] [instr names]]
                                                   (if names
                                                     [(+ 1 l) (conj p [l (str instr " ; " (string/join ", " names))]) (merge m (into {} (map (fn [n] {n l}) names)))]
                                                     [(+ 1 l) (conj p [l instr]) m]
                                                     ))
                                                 [0 [] {}]
                                                 ordered-flattened-lams)
        result (map (fn [[l instr]]
                      (let [num-replaced (string/replace instr #"@(\d+)" (fn [[_ n]] (str (+ l (string->number n)))))
                            names-replaced (string/replace num-replaced #"@([^ ]+)" (fn [[_ n]]
                                                                                           (when-not (names-lines n) (throw (Exception. (str "Unknown name: [" n "]"))))
                                                                                           (str (names-lines n))))]
                        [l names-replaced]))
                    p-ast-with-lines)

        ]
    (vec result)))

(defn gcc [defuns]
  (let [rewritten-asts (map rewrite-sexp defuns)
        asts (map #(compile-sexp-to-gcc % {} {}) rewritten-asts)
        all (apply merge (map #(:lambdas %) asts))
        ast-wl (add-lines all)
        out (string/join "\n" (flatten (map (fn [[_ instr]] [instr]) ast-wl)))]
    out))

(defn -main [& args]
  (let [in  (apply concat (map (fn [f] (read-string (slurp f))) args))
        out (gcc in)
        out-file (spit "lambdaman.gcc" out)]
    (println (format "input:\n====================\n%s\n====================" (string/join "\n--------------------\n" in)))
    (println (format "output:\n====================\n%s\n====================" out))
    (println "wrote output to lambdaman.gcc")
    (println "good luck. 🙋")))
