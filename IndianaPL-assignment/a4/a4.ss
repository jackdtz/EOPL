#lang racket


(define lex
  (lambda (exp env)

    (define env0
      '([zero?     .  (zero?  1)]
        (sub1     .  (sub1   1))))
    
    (match exp
      [(? number? num) `(const ,num)]
      [(? symbol? sym) (let ([index (get-index sym env)])
                         (cond [(not (= index (- 1))) `(var ,index)]
                               [else
                                (let ([member-of? (assq sym env0)])
                                  (if member-of?
                                      sym
                                      `(freevar ,sym)))]))]                 
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x env)))]
      [`(,f ,arg) `(,(lex f env) ,(lex arg env))]
      [`(zero? ,e) `(zero? ,(lex e env))]
      [`(sub1 ,e) `(sub1 ,(lex e env))]
      [`(if ,test ,conseq ,alt) `(if ,(lex test env)
                                     ,(lex conseq env)
                                     ,(lex alt env))]
      [`(* ,e1 ,e2) `(* ,(lex e1 env) ,(lex e2 env))]
      [`(let ([,id* ,val*] ...) ,body)
       (let ([env1 (append id* env)])
         `(let ,(map (lambda (val) (lex val env1)) val*) ,(lex body env1)))])))


(define get-index
  (lambda (id lst)
    (define helper
      (lambda (id lst base-index)
        (cond [(null? lst) (- 1)]
              [(equal? id (car lst)) base-index]
              [else
               (helper id (cdr lst) (+ 1 base-index))])))
    (helper id lst 0)))



(lex '((lambda (x) x) 5)  '())

(pretty-print (lex '(lambda (!)
  	  (lambda (n)
  	    (if (zero? n) 1 (* n (! (sub1 n))))))
	'()))


(pretty-print (lex '(let ((! (lambda (!)
  		   (lambda (n)
  		     (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
          ((! !) 5))
       '()))


