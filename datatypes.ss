#lang eopl

(#%provide (all-defined))

(define-datatype program program?
  (a-form (form form?)))

(define-datatype form form?
  (define-exp
    (id symbol?)
    (exp expression?))
  (a-exp
   (exp expression?))
  (identifier
   (id identifier?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (bool-val (bool boolean?))
  (boolean-exp
   (sign boolean-sign?)
   (rands (list-of expression?)))
  (let-exp
   (pairs (list-of id-exp-pair?))
   (body expression?))
  (letrec-exp
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
  (set!-exp
   (id expression?)
   (rhs-exp expression?))
  (begin-exp
    (exp-list (list-of expression?)))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?)))
  (lexvar-exp
   (depth number?)
   (position number?))
  (freevar-exp
   (id symbol?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





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
   (values (list-of vector?))))

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


(define-datatype reference reference?
  (a-ref
   (position integer?)
   (vec vector?)))


(define dereference
  (lambda (ref)
    (cases reference ref
      (a-ref (position vec)
             (vector-ref vec position)))))

(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (position vec)
             (vector-set! vec position val)))))

(define cell
  (lambda (arg)
    (a-ref 0 (vector arg))))

(define cell?
  (lambda (arg)
    (reference? arg)))

(define set-cell!
  (lambda (cell value)
    (cases reference cell
      (a-ref (position vec)
             (vector-set! vec position value)))))

(define contents
  (lambda (cell)
    (cases reference cell
      (a-ref (position vec)
             (vector-ref vec position)))))

(define array
  (lambda (init-size)
    (define iterator
      (lambda (init-size collector)
        (if (zero? init-size)
            collector
            (iterator (- init-size 1) (cons (cell 0) collector)))))
    (list->vector (iterator init-size '()))))


(define access-array-slot
  (lambda (array index)
    (vector-ref array index)))

(define array-ref
  (lambda (array position)
    (dereference (access-array-slot array position))))

(define array-set!
  (lambda (array position new-value)
    (let [(slot (access-array-slot array position))]
      (set-cell! slot new-value))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define identifier?
  (lambda (exp)
    (or (number? exp)
        (symbol? exp)
        (string? exp))))

(define is-self-evaluated?
  (lambda (exp)
    (or (number? exp)
        (string? exp))))
    

(define define-exp?
  (lambda (exp)
    (and (equal? (car exp) 'define)
         (= (length exp) 3))))

(define get-define-id cadr)
(define get-define-body caddr)


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
  (cons-op (op cons?))
  (cell-op (op cell-constructor?))
  (contents-op (op contents-op?))
  (is-cell?-op (op is-cell-op?))
  (set-cell!-op (op set-cell!-op?))
  (array-op (op array-constructor?))
  (array-ref-op (op array-ref-op?))
  (array-set!-op (op array-set!-op?)))


(define array-constructor?
  (lambda (sign)
    (equal? sign 'array)))

(define array-ref-op?
  (lambda (sign)
    (equal? sign 'array-ref)))

(define array-set!-op?
  (lambda (sign)
    (equal? sign 'array-set!)))

(define cell-constructor?
  (lambda (sign)
    (equal? sign 'cell)))

(define contents-op?
  (lambda (sign)
    (equal? sign 'contents)))

(define is-cell-op?
  (lambda (sign)
    (equal? sign 'cell?)))

(define set-cell!-op?
  (lambda (sign)
    (equal? sign 'set-cell!)))

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
    (let [(prim-op (car exp))
          (op-list '(+ - * / add1 subt1 cons list car cdr cell cell? set-cell! contents array array-ref array-set!))]
      (if (memq prim-op op-list) #t #f))))

(define let-exp?
  (lambda (exp)
    (and (equal? (car exp) 'let)
         (= 3 (length exp)))))

(define letrec-exp?
  (lambda (exp)
    (and (equal? (car exp) 'letrec)
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


(define set!-exp?
  (lambda (exp)
    (and (equal? (car exp) 'set!)
         (= (length exp) 3))))

(define get-setexp-id cadr)
(define get-setexp-rhs caddr)

(define begin-exp?
  (lambda (exp)
    (and (equal? (car exp) 'begin)
         (> (length (cdr exp)) 1))))

(define get-exp-sequence cdr)