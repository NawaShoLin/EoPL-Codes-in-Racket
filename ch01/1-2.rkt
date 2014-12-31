#lang racket
(require eopl)

(provide list-length
         nth-element
         remove-first
         remove
         occurs-free?
         subst)

;; List -> Int
(define (list-length lst)
  (define (len lst cnt)
    (cond [(null? lst) cnt]
          [else (len (cdr lst) (+ 1 cnt))]))
  (len lst 0))


;; List, Int -> Val
(define (nth-element lst n)
  (define (report-list-too-short)
    (eopl:error 'nth-element
                "~s does not have ~s element.~%" lst n))
  (define (f xs n)
    (cond [(null? xs) (report-list-too-short)]
          [(zero? n) (car xs)]
          [else (f (cdr xs) (- n 1))]))
  (f lst n))


;; List, Val -> List
(define (remove-first x xs)
  (cond [(null? xs) xs]
        [(eqv? x (car xs)) (cdr xs)]
        [else (cons (car xs)
                    (remove-first x (cdr xs)))]))


;; List, Val -> List
(define (remove x xs)
  (cond [(null? xs) xs]
        [(eqv? x (car xs)) (remove x (cdr xs))]
        [else (cons (car xs)
                    (remove x (cdr xs)))]))


;; Sym,LcExp -> Bool
(define (occurs-free? var exp)  
  (cond [(symbol? exp) (eqv? var exp)]
        [(eqv? (car exp) 'lambda)
         (and 
          (not (eqv? var (arg-of exp)))
          (occurs-free? var (caddr var)))]
        [else
         (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp)))]))

(define (arg-of lambda-exp)
    (car (cadr lambda-exp)))


;; Sym, Sym, S-list -> S-list
;(define (subst new old slist)
;  (cond [(null? slist) slist]
;        [else (cons 
;               (subst-in-sexp new old (car slist))
;               (subst new old (cdr slist)))]))

;; Sym, Sym, S-exp -> S-list
;(define (subst-in-sexp new old sexp)
;  (cond [(symbol? sexp) (cond [(eqv? sexp old) new]
;                              [else sexp])]
;        [else (subst new old sexp)]))

;; Exercise 1.12
;(define (subst new old slist)
;  (cond [(null? slist) slist]
;        [else (cons 
;               (let [(sexp (car slist))]
;                 (cond [(symbol? sexp) (cond [(eqv? sexp old) new]
;                                             [else sexp])]
;                       [else (subst new old sexp)]))
;               (subst new old (cdr slist)))]))


;; Exercise 1.13
(define (subst new old slist)
  (define (subst-in-sexp sexp)
    (cond [(symbol? sexp) (cond [(eqv? sexp old) new]
                                [else sexp])]
          [else (subst new old sexp)]))
  (map subst-in-sexp slist))