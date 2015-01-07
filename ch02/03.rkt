#lang racket

;; Ex 2.3

(define one '(one))

; for testing
(define (tree-diff t)
  (cond [(equal? t one) 1]
        [else (- (tree-diff (cadr t))
                 (tree-diff (caddr t)))]))

(define (link-tree t1 t2)
  (list 'diff t1 t2))


(define (zero)
  (link-tree one one))

(define (is-zero? t)
  (zero? (tree-diff t)))

(define (successor t)
  (diff-tree-plus t one))

(define (predecessor t)
  (let [(left (link-tree t (zero)))
        (right (link-tree one (zero)))]
    (link-tree left right)))

(define (diff-tree-plus a b)
  (let [(left (link-tree a (zero)))
        (right (link-tree (zero) b))]
    (link-tree left right)))