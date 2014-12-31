#lang racket

(require rackunit
         "1-2.rkt")

(define 1-2-tests
  (test-suite
   "1-2.rkt"
   
   (check-equal? (list-length '()) 0)
   (check-equal? (list-length '(1 2 3 a b c)) 6)
   
   (check-equal? (nth-element '(a b c) 1) 'b)
   (check-equal? (nth-element '(a b c) 0) 'a)
   
   (check-equal? (remove-first 'b '(a b c b)) '(a c b))
   (check-equal? (remove-first 'b '(e f g)) '(e f g))
   
   (check-equal? (remove 'b '(a b c b)) '(a c))
   (check-equal? (remove 'b '(e f g)) '(e f g))
   
   (check-true (occurs-free? 'x 'x))
   (check-false (occurs-free? 'x '(lambda (x) (x y))))
   (check-true (occurs-free? 'x '((lambda (x) x) (x y))))
   
   (check-equal? (subst 'a 'b '((b c) (b () d)))
                 '((a c) (a () d)))
   ))

(run-test 1-2-tests)
