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
                  (primapp-exp (divide (car exp)) (map parse-expression (cdr exp)))])))))


