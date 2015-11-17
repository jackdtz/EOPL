#lang racket


(define value-of
  (lambda (exp env)
    (match exp
      [(? boolean? bool) bool]
      [(? number? num)   num]
      [(? symbol? sym)  (unbox-update (apply-env env sym))]
      [`(zero? ,n)  (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env))
                       (value-of n2 env)]
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
      [`(set! ,id ,val) (env-set! env id (value-of val env))]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (if (symbol? rand)
                                          (apply-env env rand)
                                          (box (thunk (value-of rand env)))))])))

(define-syntax thunk
  (syntax-rules ()
    ((_) (error "nothing"))
    ((_ exp) (lambda () exp))))

(define unbox-update
  (lambda (box)
    (let ([val (thrawn (unbox box))])
      (set-box! box (thunk val))
      val)))
      

(define thrawn
  (lambda (thunk)
    (thunk)))


(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (id val-box env)
    (cons (cons id val-box) env)))

(define env-set!
  (lambda (env id val-box)
    (let ([box (apply-env env id)])
      (set-box! box val-box))))

(define apply-env
  (lambda (env id)
  (let ([id-box (assq id env)])
    (if id-box
        (cdr id-box)
        (error "unbounded id" id)))))


(define closure
  (lambda (param body env)
    (list param body env)))

(define apply-closure
  (lambda (proc arg)
    (let ([id (car proc)]
          [body (cadr proc)]
          [env1 (caddr proc)])
      (value-of body (extend-env id arg env1)))))


(define run
  (lambda (exp)
    (value-of exp (empty-env))))

(run '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x)))))