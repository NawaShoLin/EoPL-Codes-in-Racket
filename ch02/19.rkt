#lang racket
(require eopl
         rackunit)

(define (number->bintree n)
  (list n '() '()))

(define (current-element t)
  (car t))

(define (move-to-left t)
  (cadr t))

(define (move-to-right t)
  (caddr t))

(define (at-leaf? t)
  (< (length t) 2))

(define (insert-to-left num t)
  (let ([left (move-to-left t)]
        [right (move-to-right t)]
        [head (current-element t)])
    (list head (insert-to-child num left) right)))

(define (insert-to-right num t)
  (let ([left (move-to-left t)]
        [right (move-to-right t)]
        [head (current-element t)])
    (list head left (insert-to-child num right))))


(define (insert-to-child num child)
  (cond [(at-leaf? child)
         (number->bintree num)]
        [else
         (list num child '())]))


;; test
(check-equal? (number->bintree 13) '(13 () ()))

(define t1 (insert-to-right 14
                              (insert-to-left 12
                                              (number->bintree 13))))

(check-equal? t1
              '(13
                (12 () ())
                (14 () ())))

(check-equal? (move-to-left t1) '(12 () ()))

(check-true (at-leaf?
             (move-to-right (move-to-left t1))))

(check-equal? (insert-to-left 15 t1)
              '(13 (15 (12 () ()) ()) (14 () ())))