#lang eopl

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-datatype primitive primitive?
  (add (sign is-add?))
  (subtract (sign is-sub?))
  (multiply (sign is-mul?))
  (divide (sign is-div?)))

(define is-add?
  (lambda (sign)
    (equal? sign '+)))

(define is-sub?
  (lambda (sign)
    (equal? sign '-)))

(define is-mul?
  (lambda (sign)
    (equal? sign '*)))

(define is-div?
  (lambda (sign)
    (equal? sign '/)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define parse-program
  (lambda (prog)
    (a-program (parse-expression prog))))

(define parse-expression
  (lambda (exp)
    (cond [(number? exp) (lit-exp exp)]
          [(symbol? exp) (var-exp exp)]
          (else
           (cond [(is-add? (car exp))
                  (primapp-exp (add (car exp)) (map parse-expression (cdr exp)))]
                 [(is-sub? (car exp))
                  (primapp-exp (subtract (car exp)) (map parse-expression (cdr exp)))]
                 [(is-mul? (car exp))
                  (primapp-exp (multiply (car exp)) (map parse-expression (cdr exp)))]
                 [(is-div? (car exp))
                  (primapp-exp (divide (car exp)) (map parse-expression (cdr exp)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eval-program
  (lambda (prog)
    (cases program (parse-program prog)
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let [(args (eval-rands rands env))]
                     (apply-primitve prim args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-rand rand env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitve
  (lambda (prim args)
    (cases primitive prim
      (add (sign)
           (+ (car args) (cadr args)))
      (subtract (sign)
                (- (car args) (cadr args)))
      (multiply (sign)
                (* (car args) (cadr args)))
      (divide (sign)
              (/ (car args) (cadr args))))))

(define apply-env
  (lambda (env sym)
    (if (null? env)
        #f
        (let ([syms (car (car env))]
              [vals (cadr (car env))]
              [env (cdr env)])
          (let ([pos (list-find-position sym syms)])
            (if (number? pos)
                (list-ref vals pos)
                (apply-env env sym)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define list-find-position
  (lambda (symbol list-of-symbol)
    (list-index
     (lambda (syml)
       (equal? syml symbol))
     list-of-symbol)))

(define list-index
  (lambda (predicate lst)
    (cond [(null? lst) #f]
          [(predicate (car lst)) 0]
          (else
           (let [(list-offset-rest (list-index predicate (cdr lst)))]
             (if (equal? list-offset-rest #f)
                 #f
                 (+ 1 list-offset-rest)))))))





(display (eval-program '(+ 8 2)))
(display "\n")