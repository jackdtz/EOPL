#lang eopl

(#%require "datatypes.ss")
(#%provide (all-defined))

(define nil '())
(define filter
  (lambda (predicate sequence)
    (cond [(null? sequence) sequence]
          [(predicate (car sequence))
           (cons (car sequence) 
                 (filter predicate (cdr sequence)))]
          [else
           (filter predicate (cdr sequence))])))

(define set-difference
  (lambda (set-1 set-2)
    (cond [(null? set-1) set-2]
          [(null? set-2) set-1]
          [(member (car set-1) set-2) 
           (cons (car set-1)
                 (set-difference (cdr set-1) set-2))]
          [else
           (set-difference (cdr set-1) set-2)])))

(define extract-all-vars
  (lambda (exp)
    
    (define collector
      (lambda (exp vars)
        (cond [(symbol? exp)
               (if (not (memq exp vars))
                   (cons exp vars)
                   vars)]
              [(lambda-exp? exp) (collector (get-lambda-body exp) vars)]
              [else
               (append (collector (car exp) vars)
                       (collector (cadr exp) vars))])))
    (collector exp nil)))

(define occur-free?
  (lambda (var exp)
    (cases expression exp
           (lit-exp (datum) #f)
           (var-exp (id)
                    (if (equal? var id) #t #f))
           (bool-val (bool) #f)
           (lexvar-exp (depth position) #f)
           (boolean-exp (sign rands)
                        (not (memq #f (map (lambda (rand) (occur-free? var rand))
                                           rands))))
           (let-exp (pairs body) #f)
           (lambda-exp (params body)
                       (and (not (memq var params))
                            (occur-free? var body)))
           (proc-app-exp (procedure args)
                         (and (occur-free? var procedure)
                              (not (memq #f (map (lambda (arg) (occur-free? var arg))
                                                 args)))))
           (if-exp (predicate consequence alternative)
                   (and (occur-free? var predicate)
                        (occur-free? var consequence)
                        (occur-free? var alternative)))
           (primapp-exp (prim rands)
                        (not (memq #f (map (lambda (rand) (occur-free? var rand))
                                           rands)))))))



(define free-vars
  (lambda (exp)
    (filter (lambda (var) (occur-free? var exp)) (extract-all-vars exp))))





