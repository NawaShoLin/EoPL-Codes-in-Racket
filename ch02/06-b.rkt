#lang racket
(require eopl)

;; env = empty-env | envp::env
;; empty-env = '()
;; envp = '(var val)

(define (empty-env)
  '())

(define (extend-env var val env)
  (let [(envp (list var val))]
    (cons envp env)))

(define (apply-env env search-var)
  (cond [(null? env) (report-no-binding-found search-var)]
        [(valid-envp? (car env))
         (let [(var (caar env))
               (val (cadar env))]
           (cond [(eqv? var search-var) val]
                 [else (apply-env (cadr env) search-var)]))]
        [else (report-invalid-env env)]))

(define (valid-envp? p)
  (and (list? p)
       (not (null? p))
       (not (null? (cdr p)))))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))