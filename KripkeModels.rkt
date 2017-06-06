#lang racket/base

(require racket/match (for-syntax racket/base syntax/parse))

;; node = (name (~option label) (node ...))
(struct Node (name labels branchs) #:transparent)

(struct Label (raw) #:transparent)

(struct Arrow Label (domain range) #:transparent)

(struct And Label (lhs rhs) #:transparent)

(struct Or Label (lhs rhs) #:transparent)

(struct Bottum Label () #:transparent)

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

;; ⊮ is just not ⊩
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

;; showing log flag
(define show-log #f)

(define (set-show-log! sl)
  (set! show-log sl))

(define-values (print-check print-result)
  (let ([indent 0])
    
    (define (print-indent)
      (define (pi i)
        (unless (= i 0)
          (display " ")
          (pi (sub1 i))))
      (pi indent))
    
    (define (print-label l)
      (match l
        [(Label raw) raw]
        [(? symbol? s) s]))
    
    (values
     (lambda (name label) 
       (when show-log
         (print-indent)
         (display "Check: ")
         (display name)
         (display " ⊩ ")
         (displayln (print-label label))
         ;; add indent by 1
         (set! indent (add1 indent))))
     
     (lambda (name label result) 
       (when show-log
         ;; sub indent by 1
         (set! indent (sub1 indent))
         (print-indent)
         (display "Result: ")
         (display name)
         (if result (display " ⊩ ") (display " ⊮ "))
         (displayln (print-label label)))))))

(define (contain? e lst)
  (cond
    [(null? lst) #f]
    [(equal? e (car lst)) #t]
    [else (contain? e (cdr lst))]))

(define (force n label)
  (match-define (Node name labels branchs) n)
  (print-check name label)
  (define result 
    (match label
      [(And _ lhs rhs)
       (and (force n lhs) (force n rhs))]
      [(Or _ lhs rhs)
       (or (force n lhs) (force n rhs))]
      [(Arrow _ lhs rhs)
       (cond
         ;; if n forces lhs, then n has to force rhs
         [(force n lhs) (force n rhs)]
         ;; if n doesn't lhs and it has no branchs, then true
         [(null? branchs) #t]
         ;; if fail all conditions above, check all its branchs
         [else (foldl (lambda (x y) (and y (force x label))) #t branchs)])]
      ;; alwayse false
      [(Bottum _) #f]
      [(? symbol? s) (contain? s labels)]))
  (print-result name label result)
  result)

(provide define-tree ⊩ ⊮ set-show-log!)