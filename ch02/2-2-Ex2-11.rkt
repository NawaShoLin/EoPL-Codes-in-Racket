#lang racket
(require eopl)

;; env = '() | env-node::env
;; env-node = (list vars vals)

(define (empty-env) '())

(define (empty-env? env)
   (null? env))

(define (extend-env* vars vals env)
  (let [(env-node (list vars vals))]
    (cons env-node env)))

(define (extend-env var val env)
  (extend-env* (list var) (list val) env))


(define (apply-env env search-var)
  (cond [(empty-env? env) (report-no-binding-found search-var)]
        [else (let [(found
                     (apply-env-node (car env) search-var))]
                (cond [(null? found) (apply-env (cdr env) search-var)]
                      [else (car found)]))]))


; return:
;   '()  => not found
;   '(v) => found v
(define (apply-env-node node s-var)
  (define (search-in-vars vars vals)
    (cond [(null? vars) '()]
          [(eqv? (car vars) s-var) (list (car vals))]
          [else (search-in-vars (cdr vars) (cdr vals))]))
  (search-in-vars (car node) (cadr node)))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))
