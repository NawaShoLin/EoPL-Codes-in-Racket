#lang racket
(require eopl)

;; env = '() | '((var val) env)

(define (empty-env)
  '())

(define (extend-env var val env)
  (list (list var val) env))

(define (apply-env env search-var)
  (cond [(null? env) (report-no-binding-found search-var)]
        [(and (list? env) (vailid-env-pair (car env)))
         (let [(var (caar env))
               (val (cadar env))]
           (cond [(eqv? var search-var) val]
                 [else (apply-env (cadr env) search-var)]))]
        [else (report-invalid-env env)]))

(define (vailid-env-pair p)
  (and (symbol? (car p))
       (not (null? (cdr p)))))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))