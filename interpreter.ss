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
      (lexvar-exp (depth position)
                  (apply-nameless-env depth position env))
      (set!-exp (id rhs-exp)
                (cases expression id
                  (lexvar-exp (depth position)
                              (set!-nameless-env position (eval-expression rhs-exp env) env))
                  (else
                   (eopl:error "id should be lexical address" id))))
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
                              (eopl:error "Incorrct number of arguments for closure" (length ids) (car args))
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
;            environment (list of vector)             ;
;                                                     ;
;                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (vals env)
    (cons (list->vector vals) env)))


(define get-saved-env
  (lambda (params body env)
    (let [(free-vars-adds (get-free-var-lexadd body))
          (max-depth (get-max-depth body))]

      (define helper
        (lambda (free-vars-adds env-collector)
          (if (null? free-vars-adds)
              nil
              (let [(free-var-add (caar free-vars-adds))
                    (depth-count (cdar free-vars-adds))]
                (cases expression free-var-add
                  (lexvar-exp (depth position)
                              (if (not (< (length env-collector) (+ 1 (- max-depth depth))))          ; test if the variable is already include in the list
                                  (helper (cdr free-vars-adds) env-collector)
                                  (helper (cdr free-vars-adds) (cons (get-env-frame (- max-depth depth) env) env-collector))))
                  (else
                   (eopl:error "incorrect type in get-saved-env")))))))
      (helper free-vars-adds '()))))

(define get-env-frame
  (lambda (depth env)
    (cond [(null? env) (eopl:error "empty env in get-env-frame func")]
          [(zero? depth) (car env)]
          [else (get-env-frame (- depth 1) env)])))

(define apply-nameless-env
  (lambda (depth position env)
    (cond [(null? env) (eopl:error "unbound variable")]
          [(zero? depth) (vector-ref (car env) position)]
          [else (apply-nameless-env (- depth 1) position (cdr env))])))

(define set!-nameless-env
  (lambda (position new-value env)
    (let [(vec-env (list->vector env))]
      (begin (vector-set! vec-env position new-value)           
             (set! env (vector->list vec-env))
             new-value))))


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

(run '(let [(x 1)
            (y 3)]
        (let [(a 100)]
          (let [(z 9)]
            (+ x y)))))

