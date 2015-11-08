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



(define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))


(define get-free-var-lexadd
  (lambda (exp)
    (define helper
      (lambda (exp collector depth-counter)
        (cases expression exp
          (lit-exp (num) collector)
          (var-exp (id) collector)
          (bool-val (bool) collector)
          (lexvar-exp (depth position)
                      (if (> depth depth-counter)
                          (cons (cons exp depth-counter) collector)
                          collector))
          (boolean-exp (sign rands)
                       (flat-map (lambda (rand) (helper rand collector depth-counter)) rands))
          (let-exp (pairs body) collector)
          (set!-exp (id rhs-exp)
                    (flat-map (lambda (rand) (helper rand collector depth-counter)) (list id rhs-exp)))       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (lambda-exp (params body)
                      (helper body collector (+ 1 depth-counter)))
          (proc-app-exp (procedure args)
                        (append (helper procedure collector depth-counter)
                                (flat-map (lambda (arg) (helper arg collector depth-counter)) args)))
          (if-exp (pred conseq altern)
                  (flat-map (lambda (sub-exp) (helper sub-exp collector depth-counter)) (list pred conseq altern)))
          (primapp-exp (prim rands)
                       (flat-map (lambda (rand) (helper rand collector depth-counter)) rands)))))
    (helper exp '() 0)))

(define extract-all-lexvar-exp
  (lambda (exp)
    (define helper
      (lambda (exp collector)
        (cases expression exp
          (lexvar-exp (depth position) (cons exp collector))
          (boolean-exp (sign rands)
                       (flat-map (lambda (rand) (helper rand collector)) rands))
          (lambda-exp (params body)
                      (helper body collector))
          (proc-app-exp (procedure args)
                        (append (helper procedure collector)
                                (flat-map (lambda (arg) (helper arg collector)) args)))
          (if-exp (pred conseq altern)
              (flat-map (lambda (sub-exp) (helper sub-exp collector)) (list pred conseq altern)))
          (primapp-exp (prim rands)
                       (flat-map (lambda (rand) (helper rand collector)) rands))
          (else
           collector))))
    (helper exp '())))

(define max-of-list
  (lambda (lst)
    (if (null? lst)
        #f
        (fold-left (lambda (e r) (if (> e r) e r))
                   (car lst)
                   (cdr lst)))))
          

(define get-max-depth
  (lambda (exp)
    (let [(all-lex-add (extract-all-lexvar-exp exp))]
      (max-of-list
       (map (lambda (lex-add)
              (cases expression lex-add
                (lexvar-exp (depth position) depth)
                (else
                 (eopl:error "Incorrect type in get-max-depth"))))
            all-lex-add)))))
