#lang racket
(require eopl)

;; Ex 2.1

; constant
(define base 16)

; 4 basic operations
(define (big-zero) '(0))

(define (big-zero? b)
  (equal? b (big-zero)))

(define (one)
  (succ (big-zero)))
(define (one? b)
  (equal?  b (one)))

(define (succ b)
  (let [(lowest (add1 (car b)))]
    (cond [(< lowest base) (cons lowest (cdr b))]
          [(null? (cdr b)) (cons 0 (succ (big-zero)))]
          [else (cons 0
                      (succ (cdr b)))])))

(define (pred b)
  (let [(lowest (sub1 (car b)))]
    (cond [(>= lowest 0) (cons lowest (cdr b))]
          [(one? (cdr b)) (list (sub1 base))]
          [else (cons (sub1 base)
                      (pred (cdr b)))])))

; extended operations
(define (plus a b)
  (cond [(big-zero? b) a]
        [else (plus (succ a) (pred b))]))

; Allways a > b
(define (sub a b)
  (cond [(big-zero? b) a]
        [else (sub (pred a) (pred b))]))

(define (mul a b)
  (mul-tr a b (big-zero)))

(define (mul-tr a b product)
  (cond [(big-zero? b) product]
        [else (mul-tr a (pred b) (plus a product))]))

(define (factorial b)
  (fact-tr b (one)))

(define (fact-tr b product)
  (cond [(one? b) product]
        [else (fact-tr (pred b) (mul b product))]))


