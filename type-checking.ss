#lang racket


(define type-env
  '([+ . ((int int) . int)]
    [- . ((int int) . int)]
    [* . ((int int) . int)]
    [/ . ((int int) . int)]
    [add1 . ((int) . int)]
    [sub1 . ((int) . int)]
    [zero? . ((int) . bool)]))

(define is-type?
  (lambda (type)
    
    (define type-keyword '(int bool))
    (define check-type
      (lambda (type)
        (or (memq type type-keyword)
             (and (= 2 (length type))
                  (list? (car type))
                  (memq (cdr type) type-keyword)))))
    
    (if (check-type type)
        #t
        (error type "is not a type"))))

(define check-list-of-types
  (lambda (type-list)
    (for-each (lambda (type) (is-type? type)) type-list)))
                   
(define extend-env
  (lambda (ids types env)
    (letrec ([loop (lambda (ids types env)
                     (cond [(null? ids) env]
                           [else
                            (cons `(,(car ids) . ,(car types)) (extend-env (cdr ids) (cdr types) env))]))])
      (if (not (= (length ids) (length types)))
          (error "number of types does not equal to number of vars")
          (loop ids types env)))))
        
 
(define apply-tenv
  (lambda (id env)
    (let ([res (assoc id env)])
      (if res
          (cdr res)
          (error "unbound" id)))))

(define get-var-types (lambda (params) (map car params)))
(define get-var (lambda (params) (map cadr params)))

(define equal-type?
  (lambda (t1 t2)
    (if (equal? t1 t2)
        #t
        (error "type error" t1 t2))))
        


(define eval-expression-type
  (lambda (exp tenv)
    (match exp
      [(? number? x) 'int]
      [(? boolean? x) 'bool]
      [(? symbol? x) (apply-tenv x tenv)]
      [`(lambda (,args ...) ,body)
       (let ([type-list (get-var-types args)]
             [var-list (get-var args)])
         (begin (check-list-of-types type-list)
                `(,type-list ,(eval-expression-type body (extend-env var-list type-list tenv)))))]
      [`(let ([,ids ,sub-exps] ...) ,body)
       (eval-expression-type body (extend-env ids (map (lambda (sub-exp) (eval-expression-type sub-exp tenv)) sub-exps) tenv))]
      [`(letrec ([,return-types ,fun-names (lambda ([,args-types ,args-ids] ...) ,fun-bodys)] ...) ,letrec-body)
       (begin (for-each (lambda (type-list) (check-list-of-types type-list)) `(,return-types ,@args-types))
              (let ([tenv-for-body (extend-env fun-names (map (lambda (args-types return-type) `(,args-types ,@return-type)) args-types return-types) tenv)])
                (begin (for-each (lambda (args-types args-ids fun-body return-type)
                                   (equal-type? return-type (eval-expression-type fun-body (extend-env args-ids args-types tenv-for-body))))
                                 args-types args-ids fun-bodys return-types)
                       (eval-expression-type letrec-body tenv-for-body))))]     
      [`(,rator ,rands ...)
       (let ([rator-type (eval-expression-type rator tenv)]
             [rands-types (map (lambda (rand) (eval-expression-type rand tenv)) rands)])
         (let ([rator-args-types (car rator-type)]
               [rator-return-type (cdr rator-type)])
           (if (not (= (length rands-types) (length rator-args-types)))
               (error "function args does not match")
               (if (for-each (lambda (rand-type rator-arg-type) (equal-type? rand-type rator-arg-type)) rands-types rator-args-types)
                   rator-return-type
                   (error "type error")))))])))

(define prettify
  (lambda (x)
    (letrec ([loop (lambda (params col)
                     (cond [(null? params) col]
                           [(null? (cdr params)) (cons (car params) (loop (cdr params) col))]
                           [else (cons (car params) (cons '* (loop (cdr params) col)))]))])
      (if (= 1 (length x))
          (car x)
          (match x
            [`((,params ...) ,result) `(,@(reverse (loop params '())) -> ,result)])))))
  
         
                             
(define eval-type
  (lambda (exp)
     (eval-expression-type exp type-env)))



#|
(eval-type '(lambda ([int x] [int y]) (zero? x)))
(eval-type '((lambda ([int x] [int y]) (zero? x)) 3 4))
(eval-type '((lambda ([int x] [int y]) (+ x y)) 3 4))
(eval-type '(lambda ([int x] [int y])
              (let ([z 3] [k 10])
                (+ (- k z) (+ x y)))))
|#
(eval-type '(letrec ([int f (lambda ([int x] [int y]) (g x y))]
                     [int g (lambda ([int x] [int y]) (+ x y))])
              (f 2 3)))