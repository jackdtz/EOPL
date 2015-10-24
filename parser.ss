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
          [else
           	(let [(prim-info (parse-primitive exp))]
              (if (or (memq (length (cdr exp)) (cdr prim-info))
                      (equal? (cadr prim-info) '()))
                  (primapp-exp (car prim-info) (map parse-expression (cdr exp)))
                  (eopl:error "Incorrect number of parameter")))])))


(define parse-primitive
  (lambda (prim-exp)
    (cond [(is-add? (car prim-exp))		(list (add (car prim-exp)) 2)]
          [(is-sub? (car prim-exp))		(list (subtract (car prim-exp)) 1 2)]
          [(is-mul? (car prim-exp))		(list (multiply (car prim-exp)) 2)]
          [(is-div? (car prim-exp))		(list (divide (car prim-exp)) 2)]
          [(add1? (car prim-exp))		(list (add1 (car prim-exp)) 1)]
          [(subt1? (car prim-exp))		(list (subt1 (car prim-exp)) 1)]
          [(list-op? (car prim-exp)) 	(list (list-op (car prim-exp)) '())]
          [(car? (car prim-exp))		(list (car-op (car prim-exp)) 1)]
          [(cdr? (car prim-exp))		(list (cdr-op (car prim-exp)) 1)]
          [(cons? (car prim-exp))		(list (cons-op (car prim-exp)) 2)]
          [else 
           (eopl:error "unknow expression" exp)])))

