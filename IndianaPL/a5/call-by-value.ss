#lang racket

(define value-of
  (lambda (exp env)
    (match exp
      [(? boolean? bool) bool]
      [(? number? num)    num]
      [`(quote ,lst)  lst]
      [`(add1 ,n) (add1 (value-of n env))]
      [`(null? ,n) (null? (value-of n env))]
      [(? symbol? sym) (apply-env env sym)]
      [`(zero? ,n)  (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(let ([,id* ,val*] ...) ,body)
       (letrec ([loop (lambda (ids vals body env)
                        (if (null? (cdr ids))
                            (value-of body (extend-env (car ids) (value-of (car vals) env) env))
                            (loop (cdr ids) (cdr vals) body (extend-env (car ids) (value-of (car vals) env) env))))])
         (loop id* val* body env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`(lambda (,x) ,body) (closure x body env)]
      [`(set! ,id ,val) (env-set! env id val)]      
      [`(cons^ ,head ,rest) (cons (thunk (value-of head env)) (thunk (value-of rest env)))]
      [`(car^ ,lst) (thrawn (car (value-of lst env)))]
      [`(cdr^ ,lst) (thrawn (cdr (value-of lst env)))]
      [`(cons ,head ,rest) (cons (value-of head env) (value-of rest env))]
      [`(car ,lst) (car (value-of lst env)) ]
      [`(cdr ,lst) (cdr (value-of lst env))]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))



(define-syntax thunk
 (syntax-rules ()
   [(_) (error "nothing")]
   [(_ e) (lambda () e)]))

(define thrawn
  (lambda (thunk-obj)
    (thunk-obj)))
              

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda  (id arg old-env)
    (cons (vector id arg) old-env)))


(define get-id
  (lambda (vec)
    (vector-ref vec 0)))

(define get-value
  (lambda (vec)
    (vector-ref vec 1)))

(define apply-env
  (lambda (env sym)
    (cond [(null? env) (error "unbound var" sym)]
          [(equal? (get-id (car env)) sym) (get-value (car env))]
          [else
           (apply-env (cdr env) sym)])))

(define env-set!
  (lambda (env sym new-val)
     (cond [(null? env) (error "unbound var" sym)]
           [(equal? (get-id (car env)) sym) (vector-set! (car env) 1 new-val)]
           [else
            (apply-env (cdr env) sym)])))


(define closure
  (lambda (param body env)
    (list param body env)))

(define apply-closure
  (lambda (proc arg)
    (let ([id (car proc)]
          [body (cadr proc)]
          [env (caddr proc)])
      (value-of body (extend-env id arg env)))))


(define run
  (lambda (exp)
    (value-of exp (empty-env))))

(run '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))
        
   
