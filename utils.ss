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
                    (flat-map (lambda (rand) (helper rand collector depth-counter)) (list id rhs-exp)))
          (begin-exp (exp-sequence)
                     (flat-map (lambda (sub-exp) (helper sub-exp collector depth-counter)) exp-sequence))
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
          (begin-exp (exp-sequence)
                     (flat-map (lambda (sub-exp) (helper sub-exp collector)) exp-sequence))
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


; (define curried-exp
;   (lambda (exp)
;     (cases expression exp
;       (lit-exp (num) exp)
;       (var-exp (id) exp)
;       (bool-val (bool) exp)
;       (lexvar-exp (depth position) exp)
;       (boolean-exp (sign rands)
;                    (boolean-exp sign (map curried-exp rands)))
;       (let-exp (pairs body) exp)
;       (lambda-exp (params body)
;                   (currying-lambda-exp params body))
;       (set!-exp (id rhs-exp)
;                 (set!-exp id (curried-exp rhs-exp)))
;       (proc-app-exp (procedure args)
;                     (cases expression procedure
;                       (proc-app-exp (procedure args)
;                                     (proc-app-exp  (curried-exp procedure) (map curried-exp args)))
;                       (lambda-exp (params body)
;                                   (let [(curried-args (map curried-exp args))]
;                                     (curried-to-nested-pro (curried-exp procedure) curried-args)))
;                       (var-exp (id)
;                                (curried-to-nested-pro procedure args))
;                       (else
;                        (eopl:error "Incorrect type at curried-exp"))))
;       (if-exp (pred conseq altern)
;               (if-exp (curried-exp pred)
;                       (curried-exp conseq)
;                       (curried-exp altern)))
;       (primapp-exp (prim rands)
;                    (primapp-exp prim (map curried-exp rands))))))

; (define currying-lambda-exp
;   (lambda (params body)
;     (let [(curried-body (curried-exp body))]
;       (define helper
;         (lambda (params curried-body)
;           (if (or (null? params)
;                   (= 1 (length params)))
;               (lambda-exp params curried-body)
;               (lambda-exp (list (car params))
;                           (helper (cdr params) curried-body)))))
;       (helper params curried-body))))

; (define curried-to-nested-pro
;   (lambda (proc-var args)

;     (define helper
;       (lambda (proc args)
;         (if (< (length args) 2)
;             (proc-app-exp proc args)
;             (helper (proc-app-exp proc (list (car args))) (cdr args)))))
               
    
;     (cases expression proc-var
;       (var-exp (id) (helper proc-var args))
;       (lambda-exp (params body)
;                   (helper proc-var args))
;       (else
;        (eopl:error "Incorrect type at curried-to-nested-pro")))))


