#lang eopl

(#%require "datatypes.ss")
(#%provide (all-defined))

(define parse-program
  (lambda (prog)
    (a-form (parse-form prog))))


(define parse-form
  (lambda (form)
    (cond [(identifier? form) (identifier form)]
          [(define-exp? form)
           (define-exp
             (get-define-id form)
             (lex-add-calculator (let-to-lambda (parse-expression (get-define-body form)))))]
          [else
           (a-exp
            (lex-add-calculator (let-to-lambda (parse-expression form))))])))
          

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
          [(letrec-exp? exp) (letrec-exp (parse-let-pairs (get-name-val-pairs exp))
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
                      (equal? (cadr prim-info) 'inf))
                  (primapp-exp (car prim-info) (map parse-expression (cdr exp)))
                  (eopl:error "Incorrect number of parameter")))]
          [(set!-exp? exp) (set!-exp (parse-expression (get-setexp-id exp)) (parse-expression (get-setexp-rhs exp)))]
          [(begin-exp? exp) (begin-exp (map parse-expression (get-exp-sequence exp)))]
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
    (cond [(greater-sign? (car exp))                  `( ,(greater-than-sign   (car exp)) 2)]
          [(less-sign? (car exp))                     `( ,(less-than-sign      (car exp)) 2)]
          [(equal-sign? (car exp))                    `( ,(equal-sign          (car exp)) 2)]
          [(logic-and-sign? (car exp))                `( ,(logic-and-sign      (car exp)) 2)]
          [(logic-or-sign? (car exp))                 `( ,(logic-or-sign       (car exp)) 2)]
          [(logic-not-sign? (car exp))                `( ,(logic-not-sign      (car exp)) 1)]
          [(null-sign? (car exp))                     `( ,(check-null-sign     (car exp)) 1)]
          [(zero-sign? (car exp))                     `( ,(check-zero-sign     (car exp)) 1)]
          [else
           (eopl:error "Unknown boolean expression" exp)]))) 


(define parse-primitive
  (lambda (prim-exp)
    (cond [(is-add? (car prim-exp))                   `( ,(add (car prim-exp))              2)]
          [(is-sub? (car prim-exp))                   `( ,(subtract (car prim-exp))       1 2)]
          [(is-mul? (car prim-exp))                   `( ,(multiply (car prim-exp))         2)]
          [(is-div? (car prim-exp))                   `( ,(divide (car prim-exp))           2)]
          [(add1? (car prim-exp))                     `( ,(add1 (car prim-exp))             1)]
          [(subt1? (car prim-exp))                    `( ,(subt1 (car prim-exp))            1)]
          [(list-op? (car prim-exp))                  `( ,(list-op (car prim-exp))        inf)]
          [(car? (car prim-exp))                      `( ,(car-op (car prim-exp))           1)]
          [(cdr? (car prim-exp))                      `( ,(cdr-op (car prim-exp))           1)]
          [(cons? (car prim-exp))                     `( ,(cons-op (car prim-exp))          2)]
          [(cell-constructor? (car prim-exp))         `( ,(cell-op (car prim-exp))          1)]
          [(is-cell-op? (car prim-exp))               `( ,(is-cell?-op (car prim-exp))      1)]
          [(contents-op? (car prim-exp))              `( ,(contents-op (car prim-exp))      1)]
          [(set-cell!-op? (car prim-exp))             `( ,(set-cell!-op (car prim-exp))     2)]
          [(array-constructor? (car prim-exp))        `( ,(array-op (car prim-exp))         1)]
          [(array-ref-op? (car prim-exp))             `( ,(array-ref-op (car prim-exp))     2)]
          [(array-set!-op? (car prim-exp))            `( ,(array-set!-op (car prim-exp))    3)]
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
          (lexvar-exp (depth postion) ast-exp)
          (freevar-exp (id) ast-exp)
          (boolean-exp (bool-sign rands)
                       (boolean-exp bool-sign
                                    (map (lambda (rand) (helper rand env)) rands)))
          (let-exp (name-value-pairs body)
                   (eopl:error "let-exp should not exist in lex-add-calculator"))
          (letrec-exp (name-value-pairs body)
                   (eopl:error "letrec-exp should not exist in lex-add-calculator"))
          (set!-exp (id rhs-exp)
                    (set!-exp (helper id env) (helper rhs-exp env)))
          (begin-exp (exp-sequence)
                     (begin-exp (map (lambda (sub-exp) (helper sub-exp env)) exp-sequence)))
          (lambda-exp (params body)
                      (lambda-exp params (helper body (cons params env))))
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

    (define get-depth-frame
      (lambda (id lst base-index)
        
        (cond [(null? lst) (cons #f (freevar-exp id))]
              [(memq id (car lst)) (cons base-index (car lst))]
              [else
               (get-depth-frame id (cdr lst) (+ 1 base-index))])))

    (define get-position
      (lambda (symbol frame base-index)
        (if (equal? symbol (car frame))
            base-index
            (get-position symbol (cdr frame) (+ 1 base-index)))))
    
    (let [(frame-info (get-depth-frame id env 0))]
      (if (not (equal? (car frame-info) #f))
          (let [(depth (car frame-info))
                (frame (cdr frame-info))]
            (lexvar-exp depth (get-position id frame 0)))
          (cdr frame-info)))))
               

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


(define let-to-lambda
  (lambda (ast-exp)
    (cases expression ast-exp
      (lit-exp (num) ast-exp)
      (var-exp (id) ast-exp)
      (bool-val (bool) ast-exp)
      (lexvar-exp (depth postion) ast-exp)
      (freevar-exp (id) ast-exp)
      (boolean-exp (bool-sign rands) ast-exp)
      (let-exp (name-value-pairs body)
               (let [(params (extract-names name-value-pairs))
                     (args (extract-values name-value-pairs))]
                 (let [(new-body (let-to-lambda body))]
                   (proc-app-exp (lambda-exp params new-body)
                                 (map let-to-lambda args)))))
      (letrec-exp (name-value-pairs body)
                  (let [(ids (extract-names name-value-pairs))
                        (functions (extract-values name-value-pairs))]
                    (let [(new-body (add-set!-exp ids functions (let-to-lambda body)))]
                      (proc-app-exp (lambda-exp ids new-body)
                                    (map (lambda (id) (lit-exp 0)) ids)))))                 
      (set!-exp (id rhs-exp) (set!-exp id (let-to-lambda rhs-exp)))
      (begin-exp (exp-sequence) (begin-exp (map let-to-lambda exp-sequence)))
      (lambda-exp (params body) (lambda-exp params (let-to-lambda body)))
      (proc-app-exp (rator rands) (proc-app-exp (let-to-lambda rator) (map let-to-lambda rands)))
      (if-exp (pred conseq altern) (if-exp (let-to-lambda pred)
                                           (let-to-lambda conseq)
                                           (let-to-lambda altern)))
      (primapp-exp (prim rands) (primapp-exp prim (map let-to-lambda rands))))))

    
(define add-set!-exp
  (lambda (ids functions body)
    (begin-exp (append
                (map (lambda (id function) (set!-exp (parse-expression id) function)) ids functions)
                (list body)))))



(parse-program '(let [(a 3)
                      (b 4)
                      (swap (lambda (x y)
                              (let [(temp x)]
                                (begin (set! x y)
                                       (set! y temp)))))]
                  (begin (swap a b)
                         a
                         b)))