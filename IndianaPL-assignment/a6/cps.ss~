#lang racket


(define square-sum
  (lambda (x y k)
    (pass-res-to-cont (* x x) (lambda (sqr-x)
                                (pass-res-to-cont (* y y) (lambda (sqr-y)
                                                            (+ sqr-x sqr-y)))))))

(define pass-res-to-cont
  (lambda (res k)
    (k res)))



(define fact-cps
  (lambda (n k)
    (cond [(= n 0) (k 0)]
          [(= n 1) (k 1)]
          [else
           (fact-cps (- n 1) (lambda (rest-fact)
                           (k (* rest-fact n))))])))

(define fact
  (lambda (n)
    (fact-cps n display)))




(define bin-to-dec-cps
  (lambda (n k)
    (if (null? n)
        (k 0)
        (bin-to-dec-cps (cdr n) (lambda (next-ele)
                                  (k (+ (car n) (* 2 next-ele))))))))

(define binary-to-decimal
  (lambda (n)
    (bin-to-dec-cps n (lambda (x) x))))

(define times-cps
  (lambda (ls k)
    (cond [(null? ls) (k 1)]
          [(zero? (car ls)) (k 0)]
          [else
            (times-cps (cdr ls) (lambda (result-rest)
                                   (k (* (car ls) result-rest))))])))

(define times-cps-shortcut
  (lambda (ls k)
    (cond [(null? ls) (k 1)]
          [(zero? (car ls)) 0]
          [else
           (times-cps (cdr ls) (lambda (result-rest)
                                   (k (* (car ls) result-rest))))])))

(define times
  (lambda (ls)
    (times-cps-shortcut ls (lambda (x) x))))


(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (+ m n))))))


(define plus
  (lambda (m)
    (lambda (n)
      ((plus-cps m (lambda (x) x)) n (lambda (x) x)))))


(define remv-first-9-cps
  (lambda (ls k)
    (cond [(null? ls) (k '())]
          [(pair? (car ls))
           (remv-first-9-cps (car ls) (lambda (car-res)
                                        (if (equal? (car ls) car-res)
                                            (remv-first-9-cps (cdr ls) (lambda (cdr-res)
                                                                         (k (cons (car ls) cdr-res))))
                                            (remv-first-9-cps (cdr ls) (lambda (cdr-res)
                                                                         (k (cons car-res cdr-res)))))))]

          [(eqv? (car ls) 9) (k (cdr ls))]
          [else
           (remv-first-9-cps (cdr ls) (lambda (rest)
                                        (k (cons (car ls) rest))))])))




(define cons-cell-count-cps
  (lambda (ls k)
    (cond [(pair? ls)
           (cons-cell-count-cps (car ls) (lambda (car-res)
                                       (cons-cell-count-cps (cdr ls) (lambda (cdr-res)
                                                                   (k (add1 (+ car-res cdr-res)))))))]
          [else (k 0)])))

(define cons-cell-count
  (lambda (ls)
    (cons-cell-count-cps ls (lambda (x) x))))



(define find-cps
  (lambda (id lst k)
    (let ([pair (assv id lst)])
      (if pair
          (find-cps (cdr pair) lst (lambda (res)
                                     (k res)))
          (k id)))))

(define find
  (lambda (id lst)
    (find-cps id lst (lambda (x) x))))