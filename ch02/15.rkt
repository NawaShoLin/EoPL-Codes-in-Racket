#lang racket
(require eopl
         rackunit)


(define (var-exp var) var)

(define (lambda-exp var exp)
  (list 'lambda (list var) exp))

(define (app-exp exp1 exp2)
  (list exp1 exp2))


(define (var-exp? exp)
  (symbol? exp))

(define (lambda-exp? exp)
  (and [list? exp]
       [check-list-length exp 3]
       [eqv? 'lambda (car exp)]
       [lambda-id-field? (cadr exp)]
       [exp? (caddr exp)]))


(define (lambda-id-field? e)
  (and [list? e]
       [check-list-length e 1]
       [var-exp? (car e)]))


(define (app-exp? exp)
  (and [list? exp]
       [check-list-length exp 2]
       [exp? (car exp)]
       [exp? (cadr exp)]))

(define (exp? e)
  [or (var-exp? e)
      (lambda-exp? e)
      (app-exp? e)])

(define (check-list-length lst len)
  (cond [(= len 0) (null? lst)]
        [(null? lst) #f]
        [else (check-list-length (cdr lst) (sub1 len))]))

(define (var-exp->var exp)
  exp)

(define (lambda-exp->bound-var exp)
  (car (cadr exp)))

(define (lambda-exp->body exp)
  (caddr exp))

(define (app-exp->rator exp)
  (car exp))

(define (app-exp->rand exp)
  (cadr exp))

;; tests
(define ve (var-exp 'a))
(define le (lambda-exp 'funn 'xd))
(define ae (app-exp le ve))

(check-true (var-exp? ve))
(check-true (lambda-exp? le))
(check-true (app-exp? ae))

(check-true (exp? ve))
(check-true (exp? le))
(check-true (exp? ae))

(check-equal? (var-exp->var ve) 'a)

(check-equal? (lambda-exp->bound-var le) 'funn)
(check-equal? (lambda-exp->body le) 'xd)

(check-equal? (app-exp->rator ae) le)
(check-equal? (app-exp->rand ae) ve)