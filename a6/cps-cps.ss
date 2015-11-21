#lang racket

(define primitive?
  (lambda (x)
    (memq x '(zero? add1 sub1 + - * /))))

(define procedure?
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
      [`(,rator ,rands) #f])))


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
          [(and (simple-exp? exp) (procedure? cont)) `(let ([v ,(cps-simple-exp exp)])
                                                        ,(caddr cont))]
          [(procedure? exp) `(,@(map cps-simple-exp exp) ,cont)]
          [else
           (match exp
             [`(if ,h ,e1, e2) (cond [(simple-exp? h) `(if ,(cps-simple-exp h) ,(cps-exp e1 cont) ,(cps-exp e2 cont)) ]
                                     [else
                                      `(,@(cps-exp h `(lambda (v) ,(cps-exp `(if v e1 e2) cont))))])]
             [`(,(? primtive-app? prim) ,rands) (
      