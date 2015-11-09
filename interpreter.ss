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
    (cases program prog
      (a-form (a-form)
              (cases form a-form
                (a-exp (body)
                       (eval-expression body (empty-nameless-env)))
                (define-exp (id body)
                  (let [(search-res-vec (contains? id global-env))]
                    (if search-res-vec
                        (begin (vector-set! search-res-vec 1 (eval-expression body (empty-nameless-env))) nil)
                        (begin (extend-global-env id (eval-expression body (empty-nameless-env)) global-env) nil)))))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (eopl:error "var-exp should not appear " var-exp))
      (let-exp (name-value-pairs body)
               (eopl:error "let-exp should not appear " let-exp))
      (letrec-exp (name-value-pairs body)
                  (eopl:error "letrec-exp should not appear " letrec-exp))
      (lexvar-exp (depth position)
                  (apply-nameless-env depth position env))
      (freevar-exp (id) (apply-global-env id))           
      (set!-exp (id rhs-exp)
                (cases expression id
                  (lexvar-exp (depth position)
                              (setref! (apply-env-ref depth position env)
                                       (eval-expression rhs-exp env)))
                  (else
                   (eopl:error "id should be lexical address" id))))
      (begin-exp (exp-sequence)
                 (eval-sequence exp-sequence env))
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




(define eval-sequence
  (lambda (exp-sequence env)
    (cond [(null? exp-sequence) (eopl:error "begin expression requires at least one sub-exp" exp-sequence)]
          [(null? (cdr exp-sequence)) (eval-expression (car exp-sequence) env)]
          [else
           (begin (eval-expression (car exp-sequence) env)
                  (eval-sequence (cdr exp-sequence) env))])))


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


(define global-env '())

(define extend-global-env
  (lambda (id value env)
    (set! global-env (cons (vector id value) env))))

(define contains?
  (lambda (id env)
    (cond [(null? env) #f]
          [(equal? id (vector-ref (car env) 0)) (car env)]
          [else
           (contains? id (cdr env))])))
  
(define apply-global-env
  (lambda (id)
    (let [(res (contains? id global-env))]
      (if res
          (vector-ref res 1)
          (eopl:error id " is undefined")))))

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

(define apply-env-ref
  (lambda (depth position env)
    (cond [(null? env) (eopl:error "unbound variable")]
          [(zero? depth) (a-ref position (car env))]
          [else (apply-env-ref (- depth 1) position (cdr env))])))

(define apply-nameless-env
  (lambda (depth position env)
    (dereference (apply-env-ref depth position env))))

(define set!-nameless-env
  (lambda (depth position new-value env)
    (let [(frame (get-env-frame depth env))]
      (begin (vector-set! frame position new-value)
             1))))


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
      (let [(res (eval-program (parse-program (read))))]
        (if (null? res)
            (read-eval-loop)
            (begin (write res)
                   (newline)
                   (read-eval-loop)))))))
