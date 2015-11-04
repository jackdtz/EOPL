#lang eopl

(#%require "datatypes.ss")
(#%provide (all-defined))

(define nil '())

(define accumulate
  (lambda (precedure init sequence)
    (if (null? sequence)
        init
        (precedure (car sequence)
                   (accumulate precedure init (cdr sequence))))))

(define flat-map
  (lambda (procedure sequence)
    (accumulate append nil (map procedure sequence))))

(define filter
  (lambda (predicate sequence)
    (cond [(null? sequence) sequence]
          [(predicate (car sequence))
           (cons (car sequence) 
                 (filter predicate (cdr sequence)))]
          [else
           (filter predicate (cdr sequence))])))

(define get-free-var-lexadd
  (lambda (exp)
    (define helper
      (lambda (exp collector)
        (cases expression exp
          (lit-exp (num) collector)
          (var-exp (id) collector)
          (bool-val (bool) collector)
          (lexvar-exp (position)
                      (if (not (= 0 position))
                          (cons exp collector)
                          collector))
          (boolean-exp (sign rands)
                       (flat-map (lambda (rand) (helper rand collector)) rands))
          (let-exp (pairs body) collector)
          (lambda-exp (params body) collector)
          (proc-app-exp (procedure args)
                        (flat-map (lambda (arg) (helper arg collector)) args))
          (if-exp (pred conseq altern)
                  (flat-map (lambda (sub-exp) (helper sub-exp collector)) (list pred conseq altern)))
          (primapp-exp (prim rands)
                       (flat-map (lambda (rand) (helper rand collector)) rands)))))
    (helper exp '())))








