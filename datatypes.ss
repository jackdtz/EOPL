#lang eopl

(#%provide (all-defined))

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (bool-val (bool boolean?))
  (boolean-exp
   (sign boolean-sign?)
   (rands (list-of expression?)))
  (if-exp 
    (predicate expression?)
    (consequence expression?)
    (alternative expression?))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype boolean-sign boolean-sign?
  (greater-than-sign
   (sign greater-sign?))
  (less-than-sign (sign less-sign?))
  (equal-sign (sign equal-sign?))
  (logic-and-sign (sign logic-and-sign?))
  (logic-or-sign (sign logic-or-sign?))
  (logic-not-sign (sign logic-not-sign?))
  (check-null-sign (sign null-sign?)))

(define greater-sign?
  (lambda (sign)
    (equal? sign '>)))

(define less-sign?
  (lambda (sign)
    (equal? sign '<)))

(define equal-sign?
  (lambda (sign)
    (equal? sign '=)))

(define logic-and-sign?
  (lambda (sign)
    (equal? sign 'and)))

(define logic-or-sign?
  (lambda (sign)
    (equal? sign 'or)))

(define logic-not-sign?
  (lambda (sign)
    (equal? sign 'not)))

(define null-sign?
  (lambda (sign)
    (equal? sign 'null?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(define get-pred
  (lambda (exp)
    (cadr exp)))

(define get-conseq
  (lambda (exp)
    (caddr exp)))

(define get-altern
  (lambda (exp)
    (cadddr exp)))

(define if-exp?
  (lambda (exp)
    (and (equal? (car exp) 'if)
         (= 4 (length exp)))))

(define true?
  (lambda (exp)
    (not (zero? exp))))


(define boolean-exp?
  (lambda (exp)
    (let [(sign (car exp))]
      (or (equal? sign '>)
          (equal? sign '<)
          (equal? sign '=)
          (equal? sign 'not)
          (equal? sign 'and)
          (equal? sign 'or)
          (equal? sign 'null?)))))

