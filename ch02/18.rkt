#lang racket
(require eopl
         rackunit)


(define number->sequence
  (lambda (x)
    (list x '() '())))

(define current-element
  (lambda (lst)
    (car lst)))

(define lhs-list
  (lambda (lst)
    (cadr lst)))

(define rhs-list
  (lambda (lst)
    (caddr lst)))

(define move-to-left
  (lambda (lst)
    (cond [(at-left-end lst) lst]
          [else (list
                 [car (lhs-list lst)]
                 [cdr (lhs-list lst)]
                 [cons (current-element lst) (rhs-list lst)])])))

(define move-to-right
  (lambda (lst)
    (cond [(at-right-end lst) lst]
          [else (list
                 [car (rhs-list lst)]
                 [cons (current-element lst) (lhs-list lst)]
                 [cdr (rhs-list lst)])])))

(define insert-to-left
  (lambda (ele lst)
    (list [current-element lst]
          [cons ele (lhs-list lst)]
          [rhs-list lst])))

(define insert-to-right
  (lambda (ele lst)
    (list [current-element lst]
          [lhs-list lst]
          [cons ele (rhs-list lst)])))

(define at-left-end
  (lambda (lst)
    (empty? (lhs-list lst))))

(define at-right-end
  (lambda (lst)
    (empty? (rhs-list lst))))

;; test

(check-equal?
 (number->sequence 7)
 '(7 () ()))

(check-equal?
 (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)

(check-equal?
 (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
 '(5 (4 3 2 1) (6 7 8 9)))

(check-equal?
 (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
 '(7 (6 5 4 3 2 1) (8 9)))

(check-equal?
 (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
 '(6 (13 5 4 3 2 1) (7 8 9)))

(check-equal?
 (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
 '(6 (5 4 3 2 1) (13 7 8 9)))