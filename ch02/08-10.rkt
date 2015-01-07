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

;; Ex 2.8
(define (empty-env? env)
  (null? env))

;; Ex 2.9
(define (has-binding? env sym)
  (cond [(empty-env? env) #f]
        [(eqv? sym (caar env)) #t]
        [else (has-binding? (cdr env sym))]))

;; Ex 2.10
(define (extend-env* vars vals env)
  (cond [(null? vars) env]
        [else (let [(var (car vars))
                    (val (car vals))
                    (r-vars (cdr vars))
                    (r-vals (cdr vals))]
                (extend-env var
                            val
                            (extend-env* r-vars r-vals env)))]))