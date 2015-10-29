#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")
(#%provide (all-defined))

(define nil '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                     ;
;                                                     ;
;                   Interpreter                       ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-program
  (lambda (prog)
    (cases program  prog
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (bool-val (bool) bool)
      (boolean-exp (bool-sign rands)
                   (let [(args (eval-rands rands env))]
                         (apply-boolean bool-sign args)))
      (let-exp (name-value-pairs body)
               (let [(pairs (eval-name-value-pairs name-value-pairs env))]
                 (let [(ids (extract-eval-pair-ids pairs))
                       (values (extract-eval-pair-values pairs))]
                   (eval-expression body (extend-env (generate-id-value-pairs ids values) env)))))
      (lambda-exp (params body)
                  (closure params
                           body
                           (extendedenv-record env)))
      (proc-app-exp (rator rands)
                    (let [(proc-closure (eval-expression rator env))
                          (args (eval-rands rands env))]
                      (cases procedure proc-closure
                        (closure (ids body env1)
                          (if (not (= (length ids)
                                      (length args)))
                              (eopl:error "Wrong number of arguments for closure")
                              (eval-expression body (extend-env (generate-id-value-pairs ids args) (eval-environment env1))))))))
      (if-exp (pred conseq altern)
              (if (true? (eval-expression pred env))
                  (eval-expression conseq env)
                  (eval-expression altern env)))
      (primapp-exp (prim rands)
                   (let [(args (eval-rands rands env))]
                         (apply-primitve prim args))))))


(define eval-environment
  (lambda (env)
    (cases environment env
      (empty-env-record (empty-lst) '())
      (extendedenv-record (pair-lst) pair-lst))))
                          


(define eval-name-value-pairs
  (lambda (pairs env)
    (map (lambda (pair)
           (cases id-exp-pair pair
             (name-value-pair (id exp)
                              (list id (eval-expression exp env)))))
         pairs)))


(define extract-eval-pair-ids
  (lambda (pairs)
    (map (lambda (pair) (car pair)) pairs)))

(define extract-eval-pair-values
  (lambda (pairs)
    (map (lambda (pair) (cadr pair)) pairs)))




(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-rand rand env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-boolean
  (lambda (bool-sign args)
    (cases boolean-sign bool-sign
           (greater-than-sign (sign)
                              (> (car args) (cadr args)))
           (less-than-sign (sign)
                           (< (car args) (cadr args)))
           (equal-sign (sign)
                       (= (car args) (cadr args)))
           (logic-and-sign (sign)
                           (and (car args) (cadr args)))
           (logic-or-sign (sign)
                          (or (car args) (cadr args)))
           (logic-not-sign (sign)
                           (not (car args)))
           (check-null-sign (sign)
                            (null? (car args)))
           (check-zero-sign (sign)
                            (zero? (car args))))))

(define apply-primitve
  (lambda (prim args)
    (cases primitive prim
      (add (sign)
           (+ (car args) (cadr args)))
      (subtract (sign)
                (if (= (length args) 2)
                    (- (car args) (cadr args))
                    (- (car args))))
      (multiply (sign)
                (* (car args) (cadr args)))
      (divide (sign)
              (/ (car args) (cadr args)))
      (add1 (op)
            (+ 1 (car args)))
      (subt1 (op)
             (- (car args) 1))
      (list-op (op) args)
      (car-op (op)
              (let ([pair (car args)])
                (if (null? pair)
                    (eopl:error "empty lst")
                    (car pair))))
      (cdr-op (op)
              (let ([pair (car args)])
                (if (null? pair)
                    (eopl:error "empty lst")
                    (cdr pair))))
      (cons-op (op)
               (cons (car args) (cadr args))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                     ;
;                                                     ;
;                     environment                     ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init-env
  (lambda ()
    (extend-env
     '((i 1)
       (v 5)
       (x 10))
     (empty-env))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (sym-val-pairs env)
    (if (null? sym-val-pairs)
        env
        (cons (car sym-val-pairs)
              (extend-env (cdr sym-val-pairs) env)))))
          

(define apply-env
  (lambda (env sym)
    (if (null? env)
        (begin (display "Unknown Expression ")
                (eopl:error "unbound variable " sym))
        (cond [(equal? sym (caar env)) (cadar env)]
              [else
               (apply-env (cdr env) sym)]))))

(define generate-id-value-pairs
  (lambda (ids values)
    (map list ids values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                     ;
;                                                     ;
;                 read-eval-print-loop                ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define run
  (lambda (x)
    (eval-program (parse-program x))))

(define read-eval-loop
  (lambda ()
    (begin
      (display "--> ")
      (write (eval-program (parse-program (read))))
      (newline)
      (read-eval-loop))))




