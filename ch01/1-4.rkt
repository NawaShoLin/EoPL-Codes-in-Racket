#lang racket
(require eopl)

;; Ex 1.15
(define (duple n item)
  (cond [(zero? n) '()]
        [else (cons
               item
               (duple (- n 1) item))]))


;; Ex 1.16
(define (invert pairs)
  (define (invert-pair p)
    [list (cadr p) (car p)])
  (map invert-pair pairs))


;; Ex 1.17
(define (down lst)
  (map (lambda (x) (list x)) lst))


;; Ex 1.18
(define (swapper s1 s2 slist)
  (define (swap-ele e)
    (cond [(eqv? e s1) s2]
          [(eqv? e s2) s1]
          [else e]))
  (define (swap-lst lst)
    (cond [(list? lst) (map swap-lst lst)]
          [else (swap-ele lst)]))
  (swap-lst slist))


;; Ex 1.19
(define (list-set lst n new-ele)
  (cond [(null? lst) (eopl:error 'list-set "List too short")]
        [(zero? n) (cons new-ele (cdr lst))]
        [else (cons (car lst) 
                    (list-set (cdr lst) (sub1 n) new-ele))]))


;; Ex 1.20
(define (count-occurrences s slist)
  (define (sum-list lst) (apply + lst))
  (define (count-in-exp exp)
    (cond [(list? exp) (sum-list (map count-in-exp exp))]
          [else (cond [(eqv? s exp) 1]
                      [else 0])]))
  (count-in-exp slist))


;; Ex 1.21
(define (product sos1 sos2)
  (define (map-pair s)
    (map [lambda (x) (list s x)] sos2))
  (cond [(null? sos1) sos1]
        [else (apply append
                     (map map-pair sos1))]))


;; Ex 1.22
(define (filter-in pred lst)
  (define (filt-rec exp)
    (cond [(list? exp) (map filt-rec exp)]
          [(pred exp) exp]
          [else '()]))
  (flatten (filt-rec lst)))


;; Ex 1.23
(define (list-index pred lst)
  (define (list-index-iter lst index)
    (cond [(pred (car lst)) index]
          [else (list-index-iter (cdr lst) (add1 index))]))
  (list-index-iter lst 0))


;; Ex 1.24
(define (every? pred lst)
  (cond [(null? lst) #t]
        [else (and (pred (car lst))
                   (every? pred (cdr lst)))]))


;; Ex 1.25
(define (exists? pred lst)
  (cond [(null? lst) #f]
        [else (or (pred (car lst))
                   (exists? pred (cdr lst)))]))


;; Ex 1.26
(define (up list)
  (cond [(null? list) list]
        [else (let [(head (car list)) (tail (cdr list))]
                (cond [(list? head) (append head (up tail))]
                      [else (cons head (up tail))]))]))


;; Ex 1.27
(define (flatten sexp)
  (cond [(list? sexp) 
         (cond [(null? sexp) sexp]
               [else (append (flatten (car sexp))
                             (flatten (cdr sexp)))])]
        [else (list sexp)]))


;; Ex 1.28
(define (merge list-a list-b)
  (cond [(null? list-a) list-b]
        [(null? list-b) list-a]
        [else (let [(a (car list-a)) (b (car list-b))]
                (cond [(<= a b) (cons a (merge (cdr list-a) list-b))]
                      [else (merge list-b list-a)]))]))


;; Ex 1.29
(define (merge-sort lst)
  (cond [(min-list? lst) lst]
        [else (merge (merge-sort (even-elements lst))
                     (merge-sort (odd-elements lst)))]))

(define (even-elements lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (car lst))]
        [else (cons (car lst)
                    (even-elements (cddr lst)))]))

(define (odd-elements lst)
  (cond [(null? lst) '()]
        [else (even-elements (cdr lst))]))

(define (min-list? lst)
  [or (null? lst) (null? (cdr lst))])

(define (sort lst)
  (merge-sort lst))

;; Ex 1.30
(define (qsort-list pred lst)
  
  (define (left-part v lst)
    (filter [lambda (x) (pred x v)] lst))
  
  (define (right-part v lst)
    (filter [lambda (x) (not (pred x v))] lst))
  
  (define (qsort lst)
    (cond [(null? lst) '()]
          [else (let [(head (car lst))
                      (body (cdr lst))]
                  (append (qsort (left-part head body))
                          (list head)
                          (qsort (right-part head body))))]))
  (qsort lst))

(define (sort/predicate pred lst)
  (qsort-list pred lst))


;; Ex 1.31
;; Bintree ::== Int | (Sym Bintree Bintree)
(define (leaf n)
  n)
(define (leaf? e)
  (integer? e))

(define (interior-node sym tree1 tree2)
  (list sym tree1 tree2))

(define (lson tree)
  (cadr tree))
(define (rson tree)
  (caddr tree))

(define (node-symbol in-node)
  (car in-node))

(define (contents-of tree-node)
  (cond [(leaf? tree-node) tree-node]
        [else (node-symbol tree-node)]))


;; Ex 1.32
(define (double-tree tree)
  (cond [(leaf? tree) (* 2 tree)]
        [else (interior-node
               (node-symbol tree)
               (double-tree (lson tree))
               (double-tree (rson tree)))]))

;;Ex 1.33
(define (mark-leaves-with-red-depth tree)
  (red-tree 0 tree))

(define (red-interior-node? t)
  (eqv? (node-symbol t) 'red))

(define (red-in-node t depth)
  (interior-node [node-symbol t]
                 [red-tree (lson t) depth]
                 [red-tree (rson t) depth]))

(define (red-tree t depth)
  (cond [(leaf? t) depth]
        [else (cond [(red-interior-node? t)
                     (red-in-node t (add1 depth))]
                    [else (red-in-node t depth)])]))


;; Ex 1.34
(define (path x bst)
  (let [(v (node-value bst))]
    (cond [(< x v) (cons 'left
                         (path x (left-side bst)))]
          [(> x v) (cons 'right
                         (path x (right-side bst)))]
          [else '()])))

(define (node-value bst)
  (car bst))
(define (left-side bst)
  (cadr bst))
(define (right-side bst)
  (caddr bst))


;; Ex 1.35
(define (number-leaves t)
  (let [(cnt 0)]
    (define (dfs-nl t)
      (cond [(leaf? t)
             (let [(lf cnt)]
               (set! cnt (add1 cnt)) lf)]
            [else 
             (interior-node [node-symbol t]
                            [dfs-nl (lson t)]
                            [dfs-nl (rson t)])]))
    (dfs-nl t)))


;; Ex 1.36
; Someone help me..