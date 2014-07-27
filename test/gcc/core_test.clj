(ns gcc.core-test
  (:use gcc.core
        midje.sweet)
  (:require [clojure.test :refer :all]))

(defn cleanup [] (swap! lambda-counter (fn [_] 0)))

(fact "basics"
      (tp 1 nil nil) => {:result  [["LDC 1"]] :lambdas nil}
      (tp '(= 1 0) nil nil) => {:result  [["LDC 1"] ["LDC 0"] ["CEQ"]] :lambdas nil}
      (tp '(> 1 0) nil nil) => {:result [["LDC 1"] ["LDC 0"] ["CGT"]] :lambdas nil}
      (tp '(car 0) nil nil) => {:result [["LDC 0"] ["CAR"]] :lambdas nil}
      (tp '(cons 1 2) nil nil) => {:result [["LDC 1"] ["LDC 2"] ["CONS"]] :lambdas nil}
      (tp '(car 0) nil nil) => {:result [["LDC 0"] ["CAR"]] :lambdas nil}
      (tp '(cdr 0) nil nil) => {:result [["LDC 0"] ["CDR"]] :lambdas nil}
      (tp '(atom? 0) nil nil) => {:result [["LDC 0"] ["ATOM"]] :lambdas nil}
      (cleanup))

(fact "lambda application"
      (tp '((lambda (i) (- i 1)) 2)
          nil
          nil) => {:result  [["LDC 2"]
                             ["LDF @$lambda-1"]
                             ["AP 1"]]
                   :lambdas {"$lambda-1"
                             [["LD 0 0" ["$lambda-1"]]
                              ["LDC 1"]
                              ["SUB"]
                              ["RTN"]]}}
      (cleanup))

(fact "reverse"
      (tp '(defun reverse (lst)
             (fold-left lst 0 (lambda (acc next)
                                      (cons next acc))))
          nil
          nil) => {:result nil
                   :lambdas {
                             "reverse" [["LD 0 0" ["reverse"]]
                                        ["LDC 0"]
                                        ["LDF @$lambda-1"]
                                        ["LDF @fold-left"]
                                        ["AP 3"]
                                        ["RTN"]]
                             "$lambda-1" [["LD 0 1" ["$lambda-1"]]
                                          ["LD 0 0"]
                                          ["CONS"]
                                          ["RTN"]]}}
          (cleanup))

(fact "gcc reverse"
      (gcc '((defun fold-left (lst acc fun)
               (tif (atom? lst)
                    acc
                    (fold-left (cdr lst)
                               (fun acc (car lst))
                               fun)))
             (defun reverse (lst)
               (fold-left lst 0 (lambda (acc next)
                                        (cons next acc))))))
      =>
      ;; lambda-1 reverse fold-left
"LD 0 1
LD 0 0
CONS
RTN
LD 0 0
LDC 0
LDF 0
LDF 10
AP 3
RTN
LD 0 0
ATOM
TSEL 13 15
LD 0 1
RTN
LD 0 0
CDR
LD 0 1
LD 0 0
CAR
LD 0 2
AP 2
LD 0 2
LDF 10
TAP 3"
      (cleanup))

(fact "nth"
      (tp '(defun nth (lst i)
             (tif (= i 0)
                  (car lst)
                  (nth (cdr lst)
                       (- i 1))))
          nil
          nil) => {:result nil
                   :lambdas {"nth" [["LD 0 1" ["nth"]]
                                    ["LDC 0"]
                                    ["CEQ"]
                                    ["TSEL @1 @4"]
                                    ["LD 0 0"]
                                    ["CAR"]
                                    ["RTN"]
                                    ["LD 0 0"]
                                    ["CDR"]
                                    ["LD 0 1"]
                                    ["LDC 1"]
                                    ["SUB"]
                                    ["LDF @nth"]
                                    ["TAP 2"]]}}
          (cleanup))

(fact "add-lines to nth"
      (let [past (tp '(defun nth (lst i)
                        (tif (= i 0)
                             (car lst)
                             (nth (cdr lst)
                                  (- i 1))))
                     nil
                     nil)
            lams (:lambdas past)]
        (add-lines lams)) => [[0 "LD 0 1"]
                              [1 "LDC 0"]
                              [2 "CEQ"]
                              [3 "TSEL 4 7"]
                              [4 "LD 0 0"]
                              [5 "CAR"]
                              [6 "RTN"]
                              [7 "LD 0 0"]
                              [8 "CDR"]
                              [9 "LD 0 1"]
                              [10 "LDC 1"]
                              [11 "SUB"]
                              [12 "LDF 0"]
                              [13 "TAP 2"]]
        (cleanup))

(fact "gcc nth"
      (gcc '((defun nth (lst i)
               (tif (= i 0)
                    (car lst)
                    (nth (cdr lst)
                         (- i 1)))))) => "LD 0 1
LDC 0
CEQ
TSEL 4 7
LD 0 0
CAR
RTN
LD 0 0
CDR
LD 0 1
LDC 1
SUB
LDF 0
TAP 2"
                        (cleanup))

(fact "fold-left"
      (tp '(defun fold-left (lst acc fun)
             (tif (atom? lst)
                  acc
                  (fold-left (cdr lst)
                             (fun acc (car lst))
                             fun)))
          nil
          nil) => {:result nil
                   :lambdas {"fold-left"
                             [["LD 0 0" ["fold-left"]]
                              ["ATOM"]
                              ["TSEL @1 @3"]
                              ["LD 0 1"]
                              ["RTN"]
                              ["LD 0 0"]
                              ["CDR"]
                              ["LD 0 1"]
                              ["LD 0 0"]
                              ["CAR"]
                              ["LD 0 2"]
                              ["AP 2"]
                              ["LD 0 2"]
                              ["LDF @fold-left"]
                              ["TAP 3"]]}}
          (cleanup))


(fact "gcc fold-left"
      (gcc '((defun fold-left (lst acc fun)
               (tif (atom? lst)
                    acc
                    (fold-left (cdr lst)
                               (fun acc (car lst))
                               fun)))))
      => "LD 0 0
ATOM
TSEL 3 5
LD 0 1
RTN
LD 0 0
CDR
LD 0 1
LD 0 0
CAR
LD 0 2
AP 2
LD 0 2
LDF 0
TAP 3"
      (cleanup))

(fact "map"
      (tp '(defun map (lst fun)
             (reverse (fold-left lst
                        0
                        (lambda (acc next)
                                (cons (fun next) acc)))))
          nil
          nil)
      => {:result nil
          :lambdas {"map" [["LD 0 0" ["map"]]
                           ["LDC 0"]
                           ["LDF @$lambda-1"]
                           ["LDF @fold-left"]
                           ["AP 3"]
                           ["LDF @reverse"]
                           ["AP 1"]
                           ["RTN"]]
                    "$lambda-1" [["LD 0 1" ["$lambda-1"]]
                                 ["LD 1 1"]
                                 ["AP 1"]
                                 ["LD 0 0"]
                                 ["CONS"]
                                 ["RTN"]]}}
      (cleanup))
