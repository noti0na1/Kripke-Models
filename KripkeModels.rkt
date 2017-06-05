#lang racket/base

(require racket/match (for-syntax racket/base syntax/parse))

(struct Node (name labels branchs) #:transparent)

(struct Label (raw) #:transparent)

(struct Arrow Label (domain range) #:transparent)

(struct And Label (lhs rhs) #:transparent)

(struct Or Label (lhs rhs) #:transparent)

(struct Bottum Label () #:transparent)

(begin-for-syntax
(define-syntax-class node
      #:description "node defination"
      (pattern
       (name:id
        (~optional label #:defaults ([label #'()]))
        (brh:node ...))
        #:with (b ...) #'(brh.name ...))))

(define-syntax (define-tree stx)
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

(define-syntax (let-tree stx)
  (syntax-parse stx
    [(_ t b ...) #'(let-tree () t b ...)]
    [(_ p t b ...) #'(let* (let-tree0 p t) b ...)]))

(define-syntax (let-tree0 stx)
  (syntax-parse stx))

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
    [`(,lhs ... -> ,rhs ...) (Arrow lb (parse-label lhs) (parse-label rhs))]
    [`(,lhs ... ∧ ,rhs ...) (And lb (parse-label lhs) (parse-label rhs))]
    [`(,lhs ... ∨ ,rhs ...) (Or lb (parse-label lhs) (parse-label rhs))]
    [`(¬ ,l) (Arrow lb (parse-label l) (Bottum '⊥))]
    [`(,l) (parse-label l)]
    ['⊥ (Bottum '⊥)]
    [(? symbol? s) s]))

(define (print-label l)
  (match l
    [(Label raw) raw]
    [(? symbol? s) s]))

(define (contain? e lst)
  (cond
    [(null? lst) #f]
    [(equal? e (car lst)) #t]
    [else (contain? e (cdr lst))]))

(define show-log #f)

(define (set-show-log! sl)
  (set! show-log sl))

(define (force n label)
  (match-define (Node name labels branchs) n)
  ;; show log
  (when show-log
    (display "Check: ")
    (display name)
    (display " ⊩ ")
    (displayln (print-label label)))
  (define result #f)
  (set! result
        (match label
          [(And _ lhs rhs)
           (and (force n lhs) (force n rhs))]
          [(Or _ lhs rhs) (or (force n lhs) (force n rhs))]
          [(Arrow _ lhs rhs)
           (cond
             [(force n lhs) (force n rhs)]
             [(null? branchs) #t]
             [else (foldl (lambda (x y) (and y (force x label))) #t branchs)])]
          [(Bottum _) #f]
          [(? symbol? s) (contain? s labels)]))
  ;; show log
  (when show-log
    (display "Result: ")
    (display name)
    (if result (display " ⊩ ") (display " ⊮ "))
    (displayln (print-label label)))
  result)

(provide define-tree ⊩ ⊮ set-show-log!)