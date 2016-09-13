#lang racket

(define primitive?
  (lambda (x)
    (letrec ([one-arg? (lambda (x) (memq x '(zero? add1 sub1)))]
             [two-args? (lambda (x) (memq x '(+ - * /)))])
      (or (one-arg? x) (two-args? x)))))

(define fv
  (let ([n -1])
    (lambda ()
      (set! n (+ 1 n))
      (string->symbol (string-append "v" (number->string n))))))

(define list-set!
  (lambda (lst index new-val)
    (let ([vec (list->vector lst)])
      (begin (vector-set! vec index new-val)
             (vector->list vec)))))

(define one-arg-prim?
  (lambda (x) (memq x '(zero? add1 sub1))))

(define two-args-prim?
  (lambda (x) (memq x '(+ - * /))))

(define lambda-exp?
  (lambda (exp)
    (match exp
      [`(lambda (,x) ,body) #t]
      [else #f])))

(define cps
  (lambda (exp)
    `(lambda (k) ,(cps-exp exp 'k))))

(define simple-exp?
  (lambda (exp)
    (match exp
      [(or (? number? x) (? symbol? x)) #t]
      [`(,(? primitive? prim) ,rands ...) (not (memq #f (map simple-exp? rands)))]
      [`(if ,test ,conseq ,altern) (and (simple-exp? test)
                                        (simple-exp? conseq)
                                        (simple-exp? altern))]
      [`(lambda (,x) ,body) #t]
      [`(let ([,ids ,vals] ...) ,body) (and (not (memq #f (map simple-exp? vals)))
                                            (simple-exp? body))]
      [`(,rator ,rands ...) #f])))
 
(define non-simple-exp?
  (lambda (exp)
    (not (simple-exp? exp))))


(define cps-simple-exp
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [(? symbol? x) x]
      [`(,(? primitive? prim) ,rands ...) `(,prim ,@(map cps-simple-exp rands))]
      [`(if ,test ,conseq ,altern) `(if ,(cps-simple-exp test)
                                        ,(cps-simple-exp conseq)
                                        ,(cps-simple-exp altern))]
      [`(lambda (,x) ,body) `(lambda (x k) ,(cps-exp body 'k))]
      [`(let ([,ids ,vals] ...) ,body) `(let (ids ,(map cps-simple-exp vals))
                                         ,(cps-simple-exp body))]
      [`(,rator ,rands) (error "can't call on application in simple-exp")])))


(define cps-exp
  (lambda (exp cont)
    (cond [(and (simple-exp? exp) (symbol? cont)) `(,cont ,(cps-simple-exp exp))]
          [(and (simple-exp? exp) (lambda-exp? cont)) `(let ([v ,(cps-simple-exp exp)])
                                                        ,(caddr cont))]
          [(lambda-exp? exp) `(,@(map cps-simple-exp exp) ,cont)]
          [else
           (match exp
             [`(if ,h ,e1, e2) (cond [(simple-exp? h) `(if ,(cps-simple-exp h) ,(cps-exp e1 cont) ,(cps-exp e2 cont)) ]
                                     [else
                                      `(,@(cps-exp h `(lambda (v) ,(cps-exp `(if v e1 e2) cont))))])]
             [`(,(? one-arg-prim? prim) ,rand) (cps-exp rand `(lambda (v)
                                                               (cont (prim v))))]
             [`(,(? two-args-prim? prim) ,rand-1 ,rand-2) (cps-exp rand-1 `(lambda (v1)
                                                                             ,(cps-exp rand-2 `(lambda (v2)
                                                                                                 (,cont (,prim v1 v2))))))]
             [`(,rator ,rands ...)
              (if (not (simple-exp? rator))
                  (let ([v-id (fv)])
                    (cps-exp rator `(lambda (,v-id)
                                      ,(cps-exp `(,v-id ,@rands) cont))))
                  (letrec ([loop (lambda (rator rands)
                                   (let ([pos (find-not-simple-rand non-simple-exp? rands)])
                                     (if (not (= pos -1))
                                         (let ([v-id (fv)])
                                           (cps-exp (list-ref rands pos)
                                                    `(lambda (,v-id)
                                                      ,(cps-exp `(,rator ,@(list-set! rands pos v-id)) cont))))
                                         `(,(cps-simple-exp rator) ,@(map cps-simple-exp rands) ,cont))))])
                    (loop rator rands)))])])))
                            

(define find-not-simple-rand
  (lambda (predicate lst)
    (define helper
      (lambda (predicate lst index)
        (cond [(null? lst) -1]
              [(predicate (car lst)) index]
              [else
               (helper predicate (cdr lst) (+ 1 index))])))
    (helper predicate lst 0)))
                            
(cps-exp '((f a) (g b)) 'k)
