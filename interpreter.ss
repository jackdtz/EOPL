#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")
(#%require "utils.ss")
(#%provide (all-defined))

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
                 (eval-expression body (empty-nameless-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (eopl:error "var-exp should not appear " var-exp))
      (let-exp (name-value-pairs body)
               (eopl:error "let-exp should not appear " let-exp))
      (lexvar-exp (position)
                  (apply-nameless-env position env))
      (bool-val (bool) bool)
      (boolean-exp (bool-sign rands)
                   (let [(args (eval-rands rands env))]
                         (apply-boolean bool-sign args)))
      
      (lambda-exp (params body)
                  (closure params
                           body
                           (let [(saved-env (get-saved-env params body env))]
                             (extendedenv-record env))))
      (proc-app-exp (rator rands)
                    (let [(proc-closure (eval-expression rator env))
                          (args (eval-rands rands env))]
                      (cases procedure proc-closure
                        (closure (ids body env1)
                          (if (not (= (length ids)
                                      (length args)))
                              (eopl:error "Wrong number of arguments for closure")
                              (eval-expression body (extend-nameless-env args (eval-environment env1))))))))
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
    (flat-map (lambda (pair)
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
;            environment (flat env --- list)          ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (vals env)
    (cons (car vals) env)))

 (define get-target-vec
      (lambda (depth lst counter)
        (if (= counter depth)
            (car lst)
            (get-target-vec depth (cdr lst) (+ counter 1)))))

(define get-saved-env
  (lambda (params body env)
    nil))

(define apply-nameless-env
  (lambda (position env)
    (cond [(null? env) (eopl:error "unbound variable")]
          [(zero? position) (car env)]
          [else (apply-nameless-env (- position 1) (cdr env))])))


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



(display (run '(((lambda (x)
                   (lambda (y)
                     (+ x y))) 1) 2)))

(newline)
