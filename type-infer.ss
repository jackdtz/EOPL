#lang racket


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
                       (compound-type? x)))))

(define atomic-type? (lambda (t) (memq t type-keyword)))
(define compound-type?
  (lambda (t)
    (if (not (pair? t))
        #f
        (and (= (length t) 2)
             (for-each (lambda (t) (typed? t)) (car t))
             (typed? (cdr t))))))

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
              [`(lambda (,ids ...) ,body)
               (let* ([var-types (map var ids)]
                      [env* (extend-env ids var-types tenv)])
                 `(,var-types . ,(infer1 body env*)))]
              [`(,rator ,rands ...)
               (let* ([rator-type (infer1 rator env)]
                      [rands-type (map (lambda (rand) (infer1 rand env)) rands)]
                      [result-type (var (fresh-var))])
                 (begin (check-equal? rator-type `(,rands-type . ,result-type))
                        result-type))]))]
         [check-equal?
          (lambda (t1 t2)
            (cond [(eqv? t1 t2)]
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
                          (set!-var-type v t))]))]
         
         [prettify
          (lambda (res)
            (letrec ([replace-if-not-type 
                      (lambda (t env)
                        (cond [(atomic-type? t) t]
                              [else
                               (let ([slot (assq t env)])
                                 (if slot (cdr slot) t))]))]
                     [loop (lambda (t env col)
                             (cond [(null? t) (values env col)]
                                   [(atomic-type? (car t)) (loop (cdr t) env (cons '* (cons (car t) col)))]
                                   [(var? (car t))
                                    (let ([vat-content (var-type (car t))])
                                      (if (a
                                   [else
                                    (let ([slot (assq (car t) env)])
                                      (if slot
                                          (if (null? (cdr t))
                                              (loop (cdr t) env (cons (cdr slot) col))
                                              (loop (cdr t) env (cons '* (cons (cdr slot) col))))
                                          (let ([new-var (name)])
                                            (if (null? (cdr t))
                                                (loop (cdr t) (cons `(,(car t) ,new-var) env) (cons new-var col))
                                                (loop (cdr t) (cons `(,(car t) ,new-var) env) (cons '* (cons new-var col)))))))]))]
                     [single-type? (lambda (res) (if (and (symbol? res) (atomic-type? res)) #t #f))])
              (cond [(symbol? res) (and (atomic-type? res) res)]
                    [else
                     (letv* ([(env* col*) (loop (car res) '() '())])
                            (if (list? (cdr res))
                                `(,(reverse col*) -> ,(loop (cdr res) env* '()))
                                `(,(reverse col*) -> ,(if (atomic-type? (cdr res))
                                                          (cdr res)
                                                          (cdr (assq (cdr res) env*))))))])))])
      (let ([res (infer1 exp tenv)])
        (prettify res)))))


      
(infer '(lambda (v) v))

(infer '(lambda (x y) (+ x y)))
(infer '(lambda (x y) (zero? x)))
(infer '((lambda (x y) (zero? x)) 3 4))
(infer '((lambda (x y) (+ x y)) 3 4))
