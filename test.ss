#lang eopl

(#%require "parser.ss")
(#%require "datatypes.ss")
(#%require "interpreter.ss")
(#%require "utils.ss")
(require rackunit)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
 "test for parsing number 1"
 (let ([exp 1])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)])
     (check-equal? ast (lit-exp 1))
     (check-equal? let-to-lambda-ast ast)
     (check-equal? final-exp let-to-lambda-ast))))

(test-case
 "test for parsing symbol a"
 (let ([exp 'a])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)])
     (check-equal? ast (var-exp 'a))
     (check-equal? let-to-lambda-ast ast)
     (check-equal? final-exp (freevar-exp exp)))))

(test-case
 "test for parsing symbol double nested let-exp"
 (let ([exp '(let [(a 3)]
               (let [(b 4)]
                 (+ a b)))])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)]
          [expected-ast (let-exp `(,(name-value-pair 'a (lit-exp 3)))
                                 (let-exp `(,(name-value-pair 'b (lit-exp 4)))
                                          (primapp-exp (add '+) `(,(var-exp 'a) ,(var-exp 'b)))))]
          [expected-let-to-lambda-ast (proc-app-exp
                                       (lambda-exp `(a)
                                                   (proc-app-exp
                                                    (lambda-exp `(b)
                                                                (primapp-exp (add '+) `(,(var-exp 'a) ,(var-exp 'b))))
                                                    `(,(lit-exp 4))))
                                       `(,(lit-exp 3)))]
          [expected-final-ast (proc-app-exp
                               (lambda-exp `(a)
                                           (proc-app-exp
                                            (lambda-exp `(b)
                                                        (primapp-exp (add '+) `(,(lexvar-exp 1 0) ,(lexvar-exp 0 0))))
                                            `(,(lit-exp 4))))
                               `(,(lit-exp 3)))])
     (check-equal? ast expected-ast)
     (check-equal? let-to-lambda-ast expected-let-to-lambda-ast)
     (check-equal? final-exp expected-final-ast)
     (check-equal? (run exp) 7))))


 
(test-case
 "test for parsing let-exp with a body of begin-exp"
 (let ([exp '(let [(x 1)
                   (y 3)]
               (begin (set! x 100)
                      y))])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)]
          [expected-ast (let-exp `(,(name-value-pair 'x (lit-exp 1))
                         ,(name-value-pair 'y (lit-exp 3)))
                       (begin-exp `(,(set!-exp (var-exp 'x) (lit-exp 100))
                                    ,(var-exp 'y))))]
          [expected-let-to-lambda-ast (proc-app-exp
                                       (lambda-exp `(x y)
                                                   (begin-exp `(,(set!-exp (var-exp 'x) (lit-exp 100)) ,(var-exp 'y))))
                                       `(,(lit-exp 1) ,(lit-exp 3)))]
          [expected-final-ast (proc-app-exp
                               (lambda-exp `(x y)
                                           (begin-exp `(,(set!-exp (lexvar-exp 0 0) (lit-exp 100)) ,(lexvar-exp 0 1))))
                               `(,(lit-exp 1) ,(lit-exp 3)))])
     (check-equal? ast expected-ast)
     (check-equal? let-to-lambda-ast expected-let-to-lambda-ast)
     (check-equal? final-exp expected-final-ast)
     (check-equal? (run exp) 3))))


(test-case
 "test for parsing let-exp with lambda-exp as value of a pair and body of function call"
 (let ([exp '(let [(f (lambda (x y) (+ x y)))]
               (let [(c 3)]
                 (f c 9)))])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)]
          [expected-ast (let-exp `(,(name-value-pair 'f
                                                     (lambda-exp '(x y) (primapp-exp (add '+) `(,(var-exp 'x) ,(var-exp 'y))))))
                                 (let-exp `(,(name-value-pair 'c (lit-exp 3)))
                                          (proc-app-exp (var-exp 'f) `(,(var-exp 'c) ,(lit-exp 9)))))]
          [expected-let-to-lambda-ast (proc-app-exp
                                       (lambda-exp `(f)
                                                   (proc-app-exp
                                                    (lambda-exp `(c)
                                                                (proc-app-exp (var-exp 'f) `(,(var-exp 'c) ,(lit-exp 9))))
                                                    `(,(lit-exp 3))))
                                       `(,(lambda-exp `(x y) (primapp-exp (add '+) `(,(var-exp 'x) ,(var-exp 'y))))))]
                                                   
          [expected-final-ast (proc-app-exp
                               (lambda-exp `(f)
                                           (proc-app-exp
                                            (lambda-exp `(c)
                                                        (proc-app-exp (lexvar-exp 1 0) `(,(lexvar-exp 0 0) ,(lit-exp 9))))
                                            `(,(lit-exp 3))))
                               `(,(lambda-exp `(x y) (primapp-exp (add '+) `(,(lexvar-exp 0 0) ,(lexvar-exp 0 1))))))])
     (check-equal? ast expected-ast)
     (check-equal? let-to-lambda-ast expected-let-to-lambda-ast)
     (check-equal? final-exp expected-final-ast)
     (check-equal? (run exp) 12))))   

(test-case
 "test for parsing let-exp with lambda-exp as value of a pair and body of function call"
 (let ([exp '(let [(swap (lambda (x y)
                           (let [(temp x)]
                             (begin (set! x y)
                                    (set! y temp)))))
                   (a 3)
                   (b 4)]
               (begin (swap a b) b))])
   (let* ([ast (parse-expression exp)]
          [let-to-lambda-ast (let-to-lambda ast)]
          [final-exp (lex-add-calculator let-to-lambda-ast)]
          [expected-ast (let-exp `(,(name-value-pair 'swap
                                                     (lambda-exp '(x y)
                                                                 (let-exp `(,(name-value-pair 'temp (var-exp 'x)))
                                                                          (begin-exp
                                                                            `(,(set!-exp (var-exp 'x) (var-exp 'y))
                                                                              ,(set!-exp (var-exp 'y) (var-exp 'temp)))))))
                                   ,(name-value-pair 'a (lit-exp 3))
                                   ,(name-value-pair 'b (lit-exp 4)))
                                 (begin-exp `(,(proc-app-exp (var-exp 'swap) `(,(var-exp 'a) ,(var-exp 'b)))
                                              ,(var-exp 'b))))]
          [expected-let-to-lambda-ast (proc-app-exp
                                       (lambda-exp `(swap a b) (begin-exp `(,(proc-app-exp (var-exp 'swap) `(,(var-exp 'a) ,(var-exp 'b))) ,(var-exp 'b))))
                                       `(,(lambda-exp `(x y)
                                                      (proc-app-exp
                                                       (lambda-exp `(temp)
                                                                   (begin-exp
                                                                     `(,(set!-exp (var-exp 'x) (var-exp 'y))
                                                                       ,(set!-exp (var-exp 'y) (var-exp 'temp)))))
                                                       `(,(var-exp 'x))))
                                       ,(lit-exp 3)
                                       ,(lit-exp 4)))]
          [expected-final-ast (proc-app-exp
                                       (lambda-exp `(swap a b) (begin-exp `(,(proc-app-exp (lexvar-exp 0 0) `(,(lexvar-exp 0 1) ,(lexvar-exp 0 2))) ,(lexvar-exp 0 2))))
                                       `(,(lambda-exp `(x y)
                                                      (proc-app-exp
                                                       (lambda-exp `(temp)
                                                                   (begin-exp
                                                                     `(,(set!-exp (lexvar-exp 1 0) (lexvar-exp 1 1))
                                                                       ,(set!-exp (lexvar-exp 1 1) (lexvar-exp 0 0)))))
                                                       `(,(lexvar-exp 0 0))))
                                         ,(lit-exp 3)
                                         ,(lit-exp 4)))])
     (check-equal? ast expected-ast)
     (check-equal? let-to-lambda-ast expected-let-to-lambda-ast)
     (check-equal? final-exp expected-final-ast)
     (check-equal? (run exp) 4))))   



(check-equal? (run '(let [(a 3)]
                      (let [(b 4)]
                        (let [(c 5)]
                          (+ a c)))))
              8
              "test for evaluating triple nested let-exp")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 4))))
              24
              "factorial function fact 4")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 0))))
              1
              "factorial function fact 1")

(check-equal? (run '(let [(make-fact (lambda (func-maker n result)
                                       (if (> n 0)
                                           (func-maker func-maker (- n 1) (* n result))
                                           result)))]
                      (let [(fact (lambda (x) (make-fact make-fact x 1)))]
                        (fact 10))))
              3628800
              "factorial function fact 1")

(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 0))))
              0
              "test for odd? 0")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 99))))
              1
              "test for odd? 99")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (odd? 108))))
              0
              "test for odd? 108")



(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 108))))
              1
              "test for even? 108")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 0))))
              1
              "test for even? 0")


(check-equal? (run '(let [(make-even (lambda (pred-1 pred-2 n)
                                       (if (zero? n)
                                           1
                                           (pred-2 pred-2 pred-1 (- n 1)))))
                          (make-odd (lambda (pred-1 pred-2 n)
                                      (if (zero? n)
                                          0
                                          (pred-2 pred-2 pred-1 (- n 1)))))]
                      (let [(odd? (lambda (x) (make-odd make-odd make-even x)))
                            (even? (lambda (x) (make-even make-even make-odd x)))]
                        (even? 99))))
              0
              "test for even? 99")


(check-equal? (run '(let ([a 3]
                          [b 4]
                          [swap (lambda (x y)
                                  (let ([temp (dereference x)])
                                    (begin
                                      (setref! x (dereference y))
                                      (setref! y temp))))])
                      (begin
                        (swap (ref a) (ref b))
                        (- a b))))
              1
              "test for ref function")


                   

                                      
