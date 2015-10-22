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
  (divide (sign is-div?)))

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
