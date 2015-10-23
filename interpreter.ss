#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")

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
      (primapp-exp (prim rands)
                   (let [(args (eval-rands rands env))]
                     (if (equal? (car args) #f)
                         exp
                         (apply-primitve prim args)))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-rand rand env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitve
  (lambda (prim args)
    (cases primitive prim
      (add (sign)
           (+ (car args) (cadr args)))
      (subtract (sign)
                (if (> (length args) 1)
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

(define apply-env
  (lambda (env sym)
    (if (null? env)
        (begin (display "Unknown Expression ")
               #f)
        (let ([syms (car (car env))]
              [vals (cadr (car env))]
              [env (cdr env)])
          (let ([pos (list-find-position sym syms)])
            (if (number? pos)
                (list-ref vals pos)
                (apply-env env sym)))))))

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

(read-eval-loop)

