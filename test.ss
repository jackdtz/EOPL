#lang eopl
(#%require "parser.ss")
(#%require "datatypes.ss")
(#%require "interpreter.ss")

(define test
  (lambda (test-function)
    (display (run test-function))
    (newline)))

; test for datatypes
(define factorial-func
  '(let [(make-fact (lambda (func-maker n result)
                     (if (> n 0)
                         (func-maker func-maker (- n 1) (* n result))
                         result)))]
     (let [(fact (lambda (x) (make-fact make-fact x 1)))]
       (fact 3))))

(define mutual-recursion-test
  '(let [(make-even (lambda (pred-1 pred-2 n)
                      (if (zero? n)
                          1
                          (pred-2 pred-2 pred-1 (- n 1)))))
      (make-odd (lambda (pred-1 pred-2 n)
                  (if (zero? n)
                      0
                      (pred-2 pred-2 pred-1 (- n 1)))))]
        (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
              (even? (lambda (x) (make-even make-even make-odd x)))]
          (odd? 3))))


(test factorial-func)
(test mutual-recursion-test)



