#lang eopl

(#%require "datatypes.ss")

(#%provide (all-defined))


(define parse-program
  (lambda (prog)
    (a-program (parse-expression prog))))

(define parse-expression
  (lambda (exp)
    (cond [(number? exp) (lit-exp exp)]
          [(symbol? exp) (var-exp exp)]
          (else
           (cond [(is-add? (car exp))
                  (primapp-exp (add (car exp)) (map parse-expression (cdr exp)))]
                 [(is-sub? (car exp))
                  (primapp-exp (subtract (car exp)) (map parse-expression (cdr exp)))]
                 [(is-mul? (car exp))
                  (primapp-exp (multiply (car exp)) (map parse-expression (cdr exp)))]
                 [(is-div? (car exp))
                  (primapp-exp (divide (car exp)) (map parse-expression (cdr exp)))]
                 [(add1? (car exp))
                  (primapp-exp (add1 (car exp)) (map parse-expression (cdr exp)))]
                 [(subt1? (car exp))
                  (primapp-exp (subt1 (car exp)) (map parse-expression (cdr exp)))]
                 [(list-op? (car exp))
                  (primapp-exp (list-op (car exp)) (map parse-expression (cdr exp)))]
                 [(car? (car exp))
                  (primapp-exp (car-op (car exp)) (map parse-expression (cdr exp)))]
                 [(cdr? (car exp))
                  (primapp-exp (cdr-op (car exp)) (map parse-expression (cdr exp)))]
                 [(cons? (car exp))
                  (primapp-exp (cons-op (car exp)) (map parse-expression (cdr exp)))]
                 [else
                  (eopl:error "unknow expression" exp)])))))


