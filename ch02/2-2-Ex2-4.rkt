#lang racket

;; Ex 2.4
(define (empty-stack) '())
(define push cons)
(define pop cdr)
(define top car)
(define empty-stack? null?)

