#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")
(#%require "interpreter.ss")
(#%require "utils.ss")
(require rackunit)




; test for datatypes
(define factorial-func
  '(let [(make-fact (lambda (func-maker n result)
                     (if (> n 0)
                         (func-maker func-maker (- n 1) (* n result))
                         result)))]
     (let [(fact (lambda (x) (make-fact make-fact x 1)))]
       (fact 4))))

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


(check-equal? (parse-expression '1) (lit-exp 1))
(check-equal? (parse-expression 'a) (var-exp 'a))
(check-equal? (parse-expression '#t) (bool-val '#t))
(check-equal? (parse-expression '(if 1 2 3)) (if-exp (lit-exp 1)
                                                     (lit-exp 2)
                                                     (lit-exp 3)))
(check-equal? (parse-expression '(let [(a 3)]
                                   (let [(b 4)]
                                     (+ a b))))
              (let-exp `(,(name-value-pair 'a (lit-exp 3)))
                       (let-exp `(,(name-value-pair 'b (lit-exp 4)))
                                (primapp-exp (add '+) `(,(var-exp 'a) ,(var-exp 'b))))))
                                    
              
(check-equal? (parse-expression '(let [(x 1)
                                       (y 3)]
                                   (begin (set! x 100)
                                          x)))
              (let-exp `(,(name-value-pair 'x (lit-exp 1))
                         ,(name-value-pair 'y (lit-exp 3)))
                       (begin-exp `(,(set!-exp (var-exp 'x) (lit-exp 100))
                                    ,(var-exp 'x)))))

(check-equal? (parse-expression '(let [(f (lambda (x y) (+ x y)))]
                                   (let [(c 3)]
                                     (f c 9))))
              (let-exp `(,(name-value-pair 'f
                                           (lambda-exp '(x y) (primapp-exp (add '+) `(,(var-exp 'x) ,(var-exp 'y))))))
                       (let-exp `(,(name-value-pair 'c (lit-exp 3)))
                                (proc-app-exp (var-exp 'f) `(,(var-exp 'c) ,(lit-exp 9))))))
