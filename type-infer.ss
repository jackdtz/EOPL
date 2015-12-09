#lang racket

; A naive type inferer. Reference: eopl, 2nd edition

(define-syntax letv*
  (syntax-rules ()
    [(_ () body ...) (begin body ...)]
    [(_ ([x0 v0] [x1 v1] ...) body ...)
     (let-values ([x0 v0])
       (letv* ([x1 v1] ...)
              body ...))]))

(define tenv
  '([+ . ((int int) . int)]
    [- . ((int int) . int)]
    [* . ((int int) . int)]
    [/ . ((int int) . int)]
    [add1 . ((int) . int)]
    [sub1 . ((int) . int)]
    [zero? . ((int) . bool)]))

(define look-up
  (lambda (x env)
    (let ([slot (assq x env)])
      (if slot
          (cdr slot)
          (error "unbound id" x env)))))

(define walk
  (lambda (x env)
    (let ([slot (assoc x env)])
      (cond [(not slot) x]
            [(var? (cdr slot)) (walk (cdr slot) env)]
            [else (cdr slot)]))))

(define extend-env
  (lambda (ids types env)
    (letrec ([loop (lambda (ids types env)
                     (cond [(null? ids) env]
                           [else
                            (cons `(,(car ids) . ,(car types)) (extend-env (cdr ids) (cdr types) env))]))])
      (if (not (= (length ids) (length types)))
          (error "number of types does not equal to number of vars")
          (loop ids types env)))))

(define ext (lambda (x v s) `((,x . ,v) . ,s)))

(define type-keyword '(int bool string))

(define typed? (lambda (t)
                 (let ([x (var-type t)])
                   (or (atomic-type? x)
                       (if (list? x)
                           (compound-type? x) #f)))))

(define atomic-type? (lambda (t) (memq t type-keyword)))
(define compound-type?
  (lambda (t)
    (if (not (pair? t))
        #f
        #t)))

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))
(define var-type (lambda (x) (vector-ref x 0)))
(define set!-var-type (lambda (x v) (vector-set! x 0 v)))

(define occur?
  (lambda (id t)
    (cond [(atomic-type? t) #f]
          [(compound-type? t)
           (begin (for-each (lambda (arg) (occur? id arg))
                            (car t))
                  (occur? id (cdr t)))]
          [else
           (let [(v-type (var-type t))]
             (if (eqv? id v-type)
                 (error "cyclic type" id t)
                 #f))])))
(define fresh-var
  (let ([serial-number -1])
    (lambda ()
      (set! serial-number (+ 1 serial-number))
      serial-number)))

(define name
  (let ([n -1])
    (lambda ()
      (begin (set! n (+ n 1))
             (string->symbol 
              (string-append "t" (number->string n)))))))

(define subt-env '())
(define ext-subt-env (lambda (x v e) (cons `(,x . ,v) e)))


(define reify
  (lambda (res)
    (define name
      (lambda (n) 
        (string->symbol 
         (string-append "t" (number->string n)))))
    (define reify1
      (lambda (x n s)
        (let ([res (walk x s)])
          (cond [(pair? res)
                 (letv* ([(u n1 s1) (reify1 (car res) n s)]
                         [(v n2 s2) (reify1 (cdr res) n1 s1)])
                        (values (cons u v) n2 s2))]
                [(var? res)
                 (let ([ct (var-type res)])
                   (cond [(or (symbol? ct) (number? ct))
                          (if (atomic-type? ct)
                              (values ct n s)
                              (let ([new-name (name n)])
                                (values new-name (+ 1 n) (ext x new-name s))))]
                         [else ; compound-type
                          (reify1 ct n s)]))]
                [else (values res n s)]))))
    (letv* ([(x1 n1 s1) (reify1 res 0 '())])
      x1)))

(define prettify
  (lambda (t)
    (letrec ([add* (lambda (lst col)
                     (cond [(null? lst) (reverse col)]
                           [(null? (cdr lst)) (add* (cdr lst) (cons (car lst) col))]
                           [else (add* (cdr lst) (cons '* (cons (car lst) col)))]))])
      (cond [(not (pair? t)) t]
            [else
             (let* ([arg (car t)]
                    [rtn (cdr t)])
               (if (= 1 (length arg))
                   `(,@(map prettify arg) -> ,(prettify rtn))
                   `(,(add* (map prettify arg) '()) -> ,(prettify rtn))))]))))
                         

(define infer
  (lambda (exp)
    (letrec
        ([infer1
          (lambda (exp env)
            (match exp
              [(? number? x) 'int]
              [(? boolean? x) 'bool]
              [(? symbol? x) (look-up x env)]
              [(? string? x) 'string]
              [`(if ,test ,conseq ,altern)
               (begin (check-equal? (infer1 test env) 'bool)
                      (let ([conseq-type (infer1 conseq env)]
                            [altern-type (infer1 altern env)])
                        (check-equal? conseq-type altern-type)
                        conseq-type))]
              [`(lambda (,ids ...) ,body)
               (let* ([var-types (map var ids)]
                      [env* (extend-env ids var-types env)])
                 `(,var-types . ,(infer1 body env*)))]
              [`(,rator ,rands ...)
               (let* ([rator-type (infer1 rator env)]
                      [rands-type (map (lambda (rand) (infer1 rand env)) rands)]
                      [result-type (var (fresh-var))])
                 (begin 
                   (let ([recc `(,rands-type . ,result-type)])
                        (check-equal? rator-type recc)
                        result-type)))]))]
         [check-equal?
          (lambda (t1 t2)
            (cond [(eq? t1 t2)]
                  [(var? t1) (check-var-equal? t1 t2)]
                  [(var? t2) (check-var-equal? t2 t1)]
                  [(and (atomic-type? t1) (atomic-type? t2))
                   (if (not (eqv? t1 t2))
                       (error "not type checked")
                       #t)]
                  [else
                   (let* ([arg-type-t1 (car t1)]
                          [arg-type-t2 (car t2)]
                          [return-type-t1 (cdr t1)]
                          [return-type-t2 (cdr t2)])
                     (if (= (length arg-type-t1) (length arg-type-t2))
                         (begin
                           (for-each
                            (lambda (t1 t2) (check-equal? t1 t2))
                            arg-type-t1 arg-type-t2)
                           (check-equal? return-type-t1 return-type-t2))
                         (error "not type checked" t1 t2)))]))]
         [check-var-equal?
          (lambda (v t)
            (cond [(typed? v) (check-equal? (var-type v) t)]
                  [else
                   (begin (occur? v t)
                          (let ([ct (var-type v)])
                            (cond [(pair? ct)
                                   (for-each (lambda (arg1 arg2) (check-equal? arg1 arg2))
                                             (car ct) (car t))
                                   (check-equal? (cdr ct) (cdr t))]
                                  [else
                                   (if (var? t)
                                       (set!-var-type v (var-type t))
                                       (set!-var-type v t))])))]))])
      (let ([res (infer1 exp tenv)])
        (prettify (reify res))))))

 
(infer '(lambda (v) v))
; => (t0 -> t0)

(infer '(lambda (x y) (+ x y)))
; => ((int * int) -> int)

(infer '(lambda (x y) (zero? x)))
; => ((int * t0) -> bool)

(infer '((lambda (x y) (zero? x)) 3 4))
; => bool

(infer '(if (zero? 1) #t #f))
; => bool

(infer '((lambda (x y) (+ x y)) 3 4))
; => int

(infer '(lambda (f) (lambda (x) (f x))))
; => ((t0 -> t1) -> (t0 -> t1))

(infer '((lambda (f) (lambda (x) (f x))) add1))
; => (int -> int)

(infer '((lambda (f) (f 1)) (lambda (v) v)))
; => int

(infer '(lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m (n f)) x))))))
; => ((t0 -> (t1 -> t2)) -> ((t3 -> t0) -> (t3 -> (t1 -> t2))))

(define S '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define K '(lambda (x) (lambda (y) x)))

(infer `(,S ,K))
; => ((t0 -> t1) -> (t0 -> t0))

(infer `((,S ,K) ,K))
; => (t0 -> t0)
