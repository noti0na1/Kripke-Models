#lang racket/base

(require racket/match (for-syntax racket/base syntax/parse))

(struct Node (name labels branchs) #:transparent)

(struct Arrow (domain range) #:transparent)

(struct And (lhs rhs) #:transparent)

(struct Or (lhs rhs) #:transparent)

(struct Bottum () #:transparent)

(define-syntax (define-tree stx)

  (define-syntax-class node
      #:description "node defination"
      (pattern
       (name:id
        (~optional label #:defaults ([label #'()]))
        (brh:node ...))
        #:with (b ...) #'(brh.name ...)))
  
  (syntax-parse stx
    [(_ t) #'(define-tree () t)]
    [(_ (p ...) n:node)
     (let ([labels (if (null? (syntax-e #'n.label))
                       #'(p ...)
                       #'(p ... (quote n.label)))])
        #`(begin
            (define-tree #,labels n.brh) ...
            (define n.name
              (Node (quote n.name)
                    (list #,@labels)
                    (list n.b ...)))))]))
    
 
(define-syntax (⊩ stx)
  (syntax-parse stx
    [(_ n:id label)
     #'(force n (parse-label (quote label)))]))

(define-syntax (⊮ stx)
  (syntax-parse stx
    [(_ n:id label)
     #'(not (force n (parse-label (quote label))))]))

(define (parse-label lb)
  (match lb
    [`(,lhs ... -> ,rhs ...) (Arrow (parse-label lhs) (parse-label rhs))]
    [`(,lhs ... ∧ ,rhs ...) (And (parse-label lhs) (parse-label rhs))]
    [`(,lhs ... ∨ ,rhs ...) (Or (parse-label lhs) (parse-label rhs))]
    [`(¬ ,l) (Arrow (parse-label l) (Bottum))]
    [`(,l) (parse-label l)]
    [(? symbol? s) s]))

(define (contain? e lst)
  (cond
    [(null? lst) #f]
    [(equal? e (car lst)) #t]
    [else (contain? e (cdr lst))]))

(define (force n label)
  (match-define (Node _ labels branchs) n)
  (match label
    [(And lhs rhs) (and (force n lhs) (force n rhs))]
    [(Or lhs rhs) (or (force n lhs) (force n rhs))]
    [(Arrow lhs rhs)
     (cond
       [(force n lhs) (force n rhs)]
       [(null? branchs) #t]
       [else (foldl (lambda (x y) (and y (force x label))) #t branchs)])]
    [(Bottum) #f]
    [(? symbol? s) (contain? s labels)]))

(provide define-tree ⊩ ⊮)