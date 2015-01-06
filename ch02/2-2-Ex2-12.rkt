#lang racket
(require eopl)


(define (empty-stack)
  (define empty?
    (lambda () #t))
  (define top
    (lambda () (eopl:error 'top "No element.")))
  (define pop
    (lambda () (eopl:error 'top "No element.")))
  (lambda (sym)
    (cond [(eqv? sym 'empty?) (empty?)]
          [(eqv? sym 'top) (top)]
          [(eqv? sym 'pop) (pop)]
          [else (eopl:error 'stack "unknow command.")])))


(define (push stack item)
  (define empty?
    (lambda () #f))
  (define top
    (lambda () item))
  (define pop
    (lambda () stack))
  (lambda (sym)
    (cond [(eqv? sym 'empty?) (empty?)]
          [(eqv? sym 'top) (top)]
          [(eqv? sym 'pop) (pop)]
          [else (eopl:error 'stack "unknow command.")])))


(define (top stack)
  (stack 'top))

(define (empty? stack)
  (stack 'empty?))

(define (pop stack)
  (stack 'pop))
