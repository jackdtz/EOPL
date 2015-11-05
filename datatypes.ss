#lang eopl

(#%provide (all-defined))

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (bool-val (bool boolean?))
  (lexvar-exp
   (position number?))
  (boolean-exp
   (sign boolean-sign?)
   (rands (list-of expression?)))
  (let-exp
   (pairs (list-of id-exp-pair?))
   (body expression?))
  (lambda-exp
    (params (list-of symbol?))
    (body expression?))
  (proc-app-exp
   (procedure expression?)
   (args (list-of expression?)))
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
  (check-null-sign (sign null-sign?))
  (check-zero-sign (sign zero-sign?)))

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

(define zero-sign?
  (lambda (sign)
    (equal? sign 'zero?)))


(define-datatype procedure procedure?
  (closure
    (ids (list-of symbol?))
    (body expression?)
    (env environment?)))

(define-datatype environment environment?
  (empty-env-record
   (empty-lst empty-lst?))
  (extendedenv-record
   (values list?)))

(define scheme-value? (lambda (v) #t))
(define empty-lst? null?)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-datatype id-exp-pair id-exp-pair?
  (name-value-pair
   (id symbol?)
   (val-exp expression?)))

(define get-pair-id car)
(define get-pair-value cadr)

(define get-name-val-pairs cadr)

(define get-let-body caddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (equal? exp #t)))


(define boolean-exp?
  (lambda (exp)
    (let [(sign (car exp))
          (sign-lst '(> < = not and or null? zero?))]
      (if (memq sign sign-lst) #t #f))))

(define primitive-exp?
  (lambda (exp)
    (let [(prim-op (car exp))]
      (or (equal? prim-op '+)
          (equal? prim-op '-)
          (equal? prim-op '*)
          (equal? prim-op '/)
          (equal? prim-op 'add1)
          (equal? prim-op 'subt1)
          (equal? prim-op 'cons)
          (equal? prim-op 'list)
          (equal? prim-op 'car)
          (equal? prim-op 'cdr)))))

(define let-exp?
  (lambda (exp)
    (and (equal? (car exp) 'let)
         (= 3 (length exp)))))

(define lambda-exp?
  (lambda (exp)
    (and (list? exp)
         (equal? (car exp) 'lambda)
         (= 3 (length exp)))))

(define get-lambda-params cadr)
(define get-lambda-body caddr)

(define proc-app-exp?
  (lambda (exp)
    (and (list? exp)
         (lambda-exp? (car exp))
         (or (= 1 (length (cdr exp)))
             (> 1 (length (cdr exp)))))))

(define get-proc-lambda car)
(define get-proc-params cdr)

(proc-app-exp? '(let [(x 5)]
                  (let [(x 8)
                        (f (lambda (y z) (* y (+ x z))))
                        (g (lambda (u) (+ u x)))]
                    (f (g 3) 17))))