#lang racket
(require "KripkeModels.rkt")

#|

define a tree with root r

           u o C
            /
           /
 s o B  t o A 
    \    /
     \  /
     r o 

|#

(define-tree
  (r ((s B ())
      (t A ((u C ()))))))

;; Note: (a . b . c) == (b a c) according to scheme syntax

(set-show-log! #t)

;; s ⊩ B => true
(s . ⊩ . B)

;; t ⊮ B => true
(t . ⊮ . B)

;; u ⊩ A => true
(u . ⊩ . A)

;; u ⊩ (A ∧ C) => true
(u . ⊩ . (A ∧ C))

;; u ⊩ (A ∧ C) => false
(u . ⊩ . (A ∧ B))

;; s ⊩ (A ∨ B) => true
(s . ⊩ . (A ∨ B))

;; s ⊩ (¬ A) => true
(s . ⊩ . (¬ A))

;; r ⊩ (A -> C) => false
(r . ⊩ . (A -> C))

;; r ⊩ (C -> A) => true
(r . ⊩ . (C -> A))
