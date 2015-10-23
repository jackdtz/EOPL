#lang eopl

(#%provide (all-defined))

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-datatype primitive primitive?
  (add (sign is-add?))
  (subtract (sign is-sub?))
  (multiply (sign is-mul?))
  (divide (sign is-div?))
  (add1 (op add1?))
  (subt1 (op subt1?))
  (list-op (op list-op?))
  (car-op (op car?))
  (cdr-op (op cdr?))
  (cons-op (op cons?)))

(define is-add?
  (lambda (sign)
    (equal? sign '+)))

(define is-sub?
  (lambda (sign)
    (equal? sign '-)))

(define is-mul?
  (lambda (sign)
    (equal? sign '*)))

(define is-div?
  (lambda (sign)
    (equal? sign '/)))

(define add1?
  (lambda (op)
    (equal? op 'add1)))

(define subt1?
  (lambda (op)
    (equal? op 'subt1)))

(define list-op?
  (lambda (op)  
    (equal? op 'list)))

(define car?
  (lambda (op)
    (equal? op 'car)))

(define cdr?
  (lambda (op)
    (equal? op 'cdr)))

(define cons?
  (lambda (op)
    (equal? op 'cons)))


