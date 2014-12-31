#lang racket
(require rackunit
         "1-3.rkt")

(define 1-3-tests
  (test-suite
   "Tests for 1-3.rkt"
 
   (check-equal? (number-element '(a b c))
                 '((a 0) (b 1) (c 3)))
 
   (check-equal? (list-sum '(1 2 3 4)) 10)
   
   (check-equal? (vector-sum #(1 2 3 4)) 10)
 
   ))


(run-test 1-3-tests)