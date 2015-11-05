#lang eopl

(#%require "datatypes.ss")
(#%provide (all-defined))


(define parse-program
  (lambda (prog)
    (a-program (lex-add-calculator (curried-exp (let-to-lambda (parse-expression prog)))))))

(define parse-expression
  (lambda (exp)
    (cond [(number? exp) (lit-exp exp)]
          [(symbol? exp) (var-exp exp)]
          [(boolean? exp) (bool-val exp)]
          [(if-exp? exp) (if-exp (parse-expression (get-pred exp))
                                 (parse-expression (get-conseq exp))
                                 (parse-expression (get-altern exp)))]
          [(let-exp? exp) (let-exp (parse-let-pairs (get-name-val-pairs exp))
                                   (parse-expression (get-let-body exp)))]
          [(lambda-exp? exp) (lambda-exp (get-lambda-params exp)
                                         (parse-expression (get-lambda-body exp)))]
          [(boolean-exp? exp)
           (let [(bool-info (parse-boolean-exp exp))]
             (if (memq (length (cdr exp)) (cdr bool-info))
                 (boolean-exp (car bool-info) (map parse-expression (cdr exp)))
                 (eopl:error "Incorrect number of parameter")))]
          [(primitive-exp? exp)
           	(let [(prim-info (parse-primitive exp))]
              (if (or (memq (length (cdr exp)) (cdr prim-info))
                      (equal? (cadr prim-info) '()))
                  (primapp-exp (car prim-info) (map parse-expression (cdr exp)))
                  (eopl:error "Incorrect number of parameter")))]
          [else
           (proc-app-exp (parse-expression (get-proc-lambda exp))
                                             (map parse-expression (get-proc-params exp)))])))


(define parse-let-pairs
  (lambda (exp)
    (define parse-pair
      (lambda (pair)
        (name-value-pair (get-pair-id pair)
                         (parse-expression (get-pair-value pair))))) 
    (map parse-pair exp)))


(define parse-boolean-exp
  (lambda (exp)
    (cond [(greater-sign? (car exp))     (list (greater-than-sign (car exp)) 2)]
          [(less-sign? (car exp))        (list (less-than-sign (car exp)) 2)]
          [(equal-sign? (car exp))       (list (equal-sign (car exp)) 2)]
          [(logic-and-sign? (car exp))   (list (logic-and-sign (car exp)) 2)]
          [(logic-or-sign? (car exp))    (list (logic-or-sign (car exp)) 2)]
          [(logic-not-sign? (car exp))   (list (logic-not-sign (car exp)) 1)]
          [(null-sign? (car exp))        (list (check-null-sign (car exp)) 1)]
          [(zero-sign? (car exp))        (list (check-zero-sign (car exp)) 1)]
          [else
           (eopl:error "Unknown boolean expression" exp)]))) 


(define parse-primitive
  (lambda (prim-exp)
    (cond [(is-add? (car prim-exp))		   (list (add (car prim-exp)) 2)]
          [(is-sub? (car prim-exp))		   (list (subtract (car prim-exp)) 1 2)]
          [(is-mul? (car prim-exp))		   (list (multiply (car prim-exp)) 2)]
          [(is-div? (car prim-exp))		   (list (divide (car prim-exp)) 2)]
          [(add1? (car prim-exp))		     (list (add1 (car prim-exp)) 1)]
          [(subt1? (car prim-exp))		   (list (subt1 (car prim-exp)) 1)]
          [(list-op? (car prim-exp)) 	   (list (list-op (car prim-exp)) '())]
          [(car? (car prim-exp))		     (list (car-op (car prim-exp)) 1)]
          [(cdr? (car prim-exp))		     (list (cdr-op (car prim-exp)) 1)]
          [(cons? (car prim-exp))		     (list (cons-op (car prim-exp)) 2)]
          [else 
           (eopl:error "unknow expression" exp)])))



(define lex-add-calculator
  (lambda (ast)
    (define helper
      (lambda (ast-exp env)
        (cases expression ast-exp
          (lit-exp (num) ast-exp)
          (var-exp (id) (get-lexical-address id env))
          (bool-val (bool) ast-exp)
          (lexvar-exp (postion) ast-exp)
          (boolean-exp (bool-sign rands) (boolean-exp bool-sign
                                                      (map (lambda (rand) (helper rand env)) rands)))
          (let-exp (name-value-pairs body)
                   (eopl:error "let-exp should not exist in lex-add-calculator"))
          (lambda-exp (params body)
                      (lambda-exp params (helper body (append params env))))
          (proc-app-exp (rator rands)
                        (proc-app-exp (helper rator env)
                                      (map (lambda (subexp) (helper subexp env))
                                           rands)))
          (if-exp (pred conseq altern)
                (if-exp (helper pred env)
                        (helper conseq env)
                        (helper altern env)))
          (primapp-exp (prim rands)
                       (primapp-exp prim
                                    (map (lambda (rand) (helper rand env))
                                         rands))))))
    (helper ast '())))

(define get-lexical-address
  (lambda (id env)

    (define get-index
      (lambda (id lst base-index)
        (if (equal? id (car lst))
            base-index
            (get-index id (cdr lst) (+ 1 base-index)))))
    
    (let [(index (get-index id env 0))]
          (lexvar-exp index))))
               

(define let-to-lambda
  (lambda (ast-exp)

    (define extract-values
      (lambda (pairs)
        (map (lambda (pair)
               (cases id-exp-pair pair
                 (name-value-pair (id value) value)))
             pairs)))

    (define extract-names
      (lambda (pairs)
        (map (lambda (pair)
               (cases id-exp-pair pair
                 (name-value-pair (id value) id)))
             pairs)))
    
    (cases expression ast-exp
      (lit-exp (num) ast-exp)
      (var-exp (id) ast-exp)
      (bool-val (bool) ast-exp)
      (lexvar-exp (position) ast-exp)
      (boolean-exp (bool-sign rands) ast-exp)
      (let-exp (name-value-pairs body)
               (let [(params (extract-names name-value-pairs))
                     (args (extract-values name-value-pairs))]
                 (let [(new-body (let-to-lambda body))]
                   (proc-app-exp (lambda-exp params new-body)
                                 args))))
      (lambda-exp (params body) ast-exp)
      (proc-app-exp (rator rands) ast-exp)
      (if-exp (pred conseq altern) ast-exp)
      (primapp-exp (prim rands) ast-exp))))


(define curried-exp
  (lambda (exp)
    (cases expression exp
           (lit-exp (num) exp)
           (var-exp (id) exp)
           (bool-val (bool) exp)
           (lexvar-exp (position) exp)
           (boolean-exp (sign rands)
                        (boolean-exp sign (map curried-exp rands)))
           (let-exp (pairs body) exp)
           (lambda-exp (params body)
                       (currying-lambda-exp params body))
           (proc-app-exp (procedure args)
                         (cases expression procedure
                           (proc-app-exp (procedure args)
                                         (proc-app-exp  (curried-exp procedure) (map curried-exp args)))
                           (lambda-exp (params body)
                                       (proc-app-exp (curried-exp procedure) (map curried-exp args)))
                           (var-exp (id)
                                    (curried-to-nested-pro procedure args))
                           (else
                            (eopl:error "Incorrect type at curried-exp"))))
           (if-exp (pred conseq altern)
                   (if-exp (curried-exp pred)
                           (curried-exp conseq)
                           (curried-exp altern)))
           (primapp-exp (prim rands)
                        (primapp-exp prim (map curried-exp rands))))))

(define currying-lambda-exp
  (lambda (params body)
    (let [(curried-body (curried-exp body))]
      (define helper
        (lambda (params curried-body)
          (if (or (null? params)
                  (= 1 (length params)))
              (lambda-exp params curried-body)
              (lambda-exp (list (car params))
                          (helper (cdr params) curried-body)))))
      (helper params curried-body))))

(define curried-to-nested-pro
  (lambda (proc-var args)

    (define helper
      (lambda (proc args)
        (if (< (length args) 2)
            (proc-app-exp proc args)
            (helper (proc-app-exp proc (list (car args))) (cdr args)))))
               
    
    (cases expression proc-var
      (var-exp (id) (helper proc-var args))
      (else
       (eopl:error "Incorrect type at curried-to-nested-pro")))))


(parse-program '(let [(make-even (lambda (pred-1 pred-2 n)
                      (if (zero? n)
                          1
                          (pred-2 pred-2 pred-1 (- n 1)))))
      (make-odd (lambda (pred-1 pred-2 n)
                  (if (zero? n)
                      0
                      (pred-2 pred-2 pred-1 (- n 1)))))]
        (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
              (even? (lambda (x) (make-even make-even make-odd x)))]
          (odd? 3))))
                      
