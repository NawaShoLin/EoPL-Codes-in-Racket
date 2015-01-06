#lang racket
(require eopl)

;; Ex 2.13 and 2.14

(define (empty-env)
  (list
   ;apply-env
   report-no-binding-found
   
   ;empty-env?
   (lambda () #t)
   
   ;has-binding?
   (lambda (sym) #f)))

(define (extend-env saved-var saved-val saved-env)
  (list
   ;apply-env
   (lambda (search-var)
      (cond [(eqv? search-var saved-var) saved-val]
            [else (apply-env saved-env search-var)]))
   
   ;empty-env?
   (lambda () #f)
   
   ;has-binding?
   (lambda (sym)
     (or (eqv? sym saved-var)
         (has-binding? saved-env sym)))))

(define (apply-env env search-var)
  ((car env) search-var))

(define (empty-env? env)
  ((cadr env)))

(define (has-binding? env sym)
  ((caddr env) sym))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))