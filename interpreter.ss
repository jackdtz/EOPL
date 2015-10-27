#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")

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
                   (eval-expression body (extend-env ids values env)))))
      (lambda-exp (params body)
                  (list params body env))
      (proc-app-exp (rator rands)
                    (cases procedure rator
                      (closure (lambda-closure environment)
                               (let [(params (car lambda-closure))
                                     (body (cadr lambda-closure))
                                     (env-1 (caddr lambda-closure))
                                     (args (eval-rands rands env))]
                                 (eval-expression body (extend-env params args env-1))))))
      (if-exp (pred conseq altern)
              (if (true? (eval-expression pred env))
                  (eval-expression conseq env)
                  (eval-expression altern env)))
      (primapp-exp (prim rands)
                   (let [(args (eval-rands rands env))]
                         (apply-primitve prim args))))))


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
                            (null? (car args))))))

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
     '(i v x)
     '(1 5 10)
     (empty-env))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
        (begin (display "Unknown Expression ")
                (eopl:error "unbound variable " sym))
        (let ([syms (car (car env))]
              [vals (cadr (car env))]
              [env (cdr env)])
          (let ([pos (list-find-position sym syms)])
            (if (number? pos)
                (list-ref vals pos)
                (apply-env env sym)))))))

(define list-find-position
  (lambda (symbol list-of-symbol)
    (list-index
     (lambda (syml)
       (equal? syml symbol))
     list-of-symbol)))

(define list-index
  (lambda (predicate lst)
    (cond [(null? lst) #f]
          [(predicate (car lst)) 0]
          (else
           (let [(list-offset-rest (list-index predicate (cdr lst)))]
             (if (equal? list-offset-rest #f)
                 #f
                 (+ 1 list-offset-rest)))))))


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


(run '(let [(x 1)]
        ((lambda (y) (+ x y)) 3)))
