#lang racket
(require eopl)

(provide number-element
         list-sum
         vector-sum)

(define (number-element lst)  
  (number-element-from lst 0))

(define (number-element-from lst n)
  (cond [(null? lst) lst]
        [else (cons
               (list n (car lst))
               (number-element-from (cdr lst) (+ n 1)))]))


(define (list-sum lst)
  (cond [(null? lst) 0]
        [else (+ (car lst) 
                 (list-sum (cdr lst)))]))


(define (partial-vector-sum v n sum)
  (cond [(zero? n) (+ (vector-ref v 0) sum)]
        [else (partial-vector-sum v 
                                  (- n 1)
                                  (+ sum (vector-ref v n)))]))


(define (vector-sum v)
  (let [(len (vector-length v))]
    (cond [(zero? len) 0]
          [else (partial-vector-sum v (- (vector-length v) 1) 0)])))