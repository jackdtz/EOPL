#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")
(#%require "interpreter.ss")
(#%require "utils.ss")
(require rackunit)



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

  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                     ;
;                                                     ;
;                parse-expression-test                ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(check-equal? (parse-expression '1)
              (lit-exp 1)
              "test for parsing number")

(check-equal? (parse-expression 'a)
              (var-exp 'a)
              "test for parsing symbol")

(check-equal? (parse-expression '#t)
              (bool-val '#t)
              "test for parsing boolean value")


(check-equal? (parse-expression '(if 1 2 3))
              (if-exp (lit-exp 1)
                      (lit-exp 2)
                      (lit-exp 3))
              "test for parsing if expression")

(check-equal? (parse-expression '(let [(a 3)]
                                   (let [(b 4)]
                                     (+ a b))))
              (let-exp `(,(name-value-pair 'a (lit-exp 3)))
                       (let-exp `(,(name-value-pair 'b (lit-exp 4)))
                                (primapp-exp (add '+) `(,(var-exp 'a) ,(var-exp 'b)))))
              "test for parsing let-expression")
                                    
              
(check-equal? (parse-expression '(let [(x 1)
                                       (y 3)]
                                   (begin (set! x 100)
                                          x)))
              (let-exp `(,(name-value-pair 'x (lit-exp 1))
                         ,(name-value-pair 'y (lit-exp 3)))
                       (begin-exp `(,(set!-exp (var-exp 'x) (lit-exp 100))
                                    ,(var-exp 'x))))
              "test for parsing let-exp with a body of begin-exp and set!-exp")

(check-equal? (parse-expression '(let [(f (lambda (x y) (+ x y)))]
                                   (let [(c 3)]
                                     (f c 9))))
              (let-exp `(,(name-value-pair 'f
                                           (lambda-exp '(x y) (primapp-exp (add '+) `(,(var-exp 'x) ,(var-exp 'y))))))
                       (let-exp `(,(name-value-pair 'c (lit-exp 3)))
                                (proc-app-exp (var-exp 'f) `(,(var-exp 'c) ,(lit-exp 9)))))
              "test for parsing let-exp with lambda-exp as value of a pair and body of function call")


(check-equal? (let-to-lambda (parse-expression '(let [(a 3)]
                                                  (let [(b 4)]
                                                    (+ a b)))))
              (proc-app-exp (lambda-exp `(a)
                                      (proc-app-exp (lambda-exp `(b) (primapp-exp (add '+) `(,(var-exp 'a) ,(var-exp 'b)))) `(,(lit-exp 4)))) `(,(lit-exp 3)))
              "test for transforming let-exp to procedure call")

(check-equal? (run '1) 1 "test for evaluating number")
(check-equal? (run "a") "a" "test for evaluating sting")
(check-equal? (run 'a) nil "test for evaluating variable")

(check-equal? (run '(let [(a 3)]
                      (let [(b 4)]
                        (+ a b))))
              7
              "test for evaluating double nested let-exp")

(check-equal? (run '(let [(a 3)]
                      (let [(b 4)]
                        (let [(c 5)]
                          (+ a c)))))
              8
              "test for evaluating triple nested let-exp")

(check-equal? (run '(let [(x 1)
                          (y 3)]
                      (begin (set! x 100)
                             x)))
              100
              "test for evaluating let-exp with body of begin-exp and set-exp")

(check-equal? (run '(let [(f (lambda (x y) (+ x y)))]
                      (let [(c 3)]
                        (f c 9))))
              12
              "test for evaluating let-exp with lambda-exp as value of a pair and body of function call")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 4))))
              24
              "factorial function fact 4")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 0))))
              1
              "factorial function fact 1")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 10))))
              3628800
              "factorial function fact 1")

(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 0))))
              0
              "test for odd? 0")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 99))))
              1
              "test for odd? 99")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 108))))
              0
              "test for odd? 108")



(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 108))))
              1
              "test for even? 108")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 0))))
              1
              "test for even? 0")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 99))))
              0
              "test for even? 99")


                   

                                      
