(ns gcc.core-test
  (:use gcc.core
        midje.sweet)
  (:require [clojure.test :refer :all]))


;; (ns gcc.core-test
;;   (:require [clojure.test :refer :all]
;;             [gcc.core :refer :all]))


(fact "transpiling"
      (tp 1 nil) => [["LDC 1"]]
      (tp '(= 1 0) nil) => [["LDC 1"] ["LDC 0"] ["CEQ"]]
      (tp '(> 1 0) nil) => [["LDC 1"] ["LDC 0"] ["CGT"]]
      (tp '(car 0) nil) => [["LDC 0"] ["CAR"]]
      (tp '(cdr 0) nil) => [["LDC 0"] ["CDR"]]
      (tp '(atom? 0) nil) => [["LDC 0"] ["ATOM"]]
      (tp '(define nth (lambda (lst i)
                                      (tif (= i 0)
                                           (car lst)
                                           (nth (cdr lst)
                                                (- i 1)))))
          nil) => [["LD 0 1" "nth"]
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
                   ["TAP 2"]]

                                        ; (cons 1 2)
                                        ; (car (cons 1 nil))= 1
                                        ; (cdr (cons 1 nil)) = '()
          (add-lines (tp '(define nth (lambda (lst i)
                                              (tif (= i 0)
                                                   (car lst)
                                                   (nth (cdr lst)
                                                        (- i 1)))))
                         nil)) => [[0 "LD 0 1"]
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

          (gcc '(define nth (lambda (lst i)
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
                                              (tp '(define fold-left (lambda (lst acc fun)
                                                                              (tif (atom? lst)
                                                                                   acc
                                                                                   (fold-left (cdr lst)
                                                                                              (fun acc (car lst))
                                                                                              fun))))
                                                  nil) => [["LD 0 0" "fold-left"]
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
                                                           ["TAP 3"]]

                                              (gcc '(define fold-left (lambda (lst acc fun)
                                                                              (tif (atom? lst)
                                                                                   acc
                                                                                   (fold-left (cdr lst)
                                                                                              (fun acc (car lst))
                                                                                              fun))))) => "LD 0 0
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

(tp '(define reverse
       (lambda (lst)
               (fold-left lst nil (lambda (acc next)
                                          (cons next acc)))))
    nil) => [
             ["LD 0 0" "reverse"]
             ["LDC 0"]
             ["LDF @lambda-1"]
             ["LDF @fold-left"]
             ["AP 3"]
             ["RTN"]
             ["LD 0 1" "lambda-1"]
             ["LD 0 0"]
             ["CONS"]
             ["RTN"]]
    ;; nil) => [
    ;;          ["LD 0 0" "reverse"]
    ;;          ["LDC 0"]
    ;;          ["LDF @lambda-1"]
    ;;          ["LDF @fold-left"]
    ;;          ["AP 3"]
    ;;          ["RTN"]
    ;;          ["LD 0 1" "lambda-1"]
    ;;          ["LD 0 0"]
    ;;          ["CONS"]
    ;;          ["RTN"]]


;; (tp '(define map (lambda (lst fun)
;;                           (fold-left lst nil (lambda (acc next)
;;                                                      (cons (fun next) acc)))))
;;     nil) => [
;;              "LD 0 0"
;;              "LDC 0"
;;              "LDF @fun"]

                                                  )
