

# Kripke Models

A Simple Racket Implement of Kripke Models

Examples in `km.rkt`:

```racket
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

;; enable log printing
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

;; u ⊩ (¬ A) => false
(u . ⊩ . (¬ A))

;; r ⊩ (A -> C) => false
(r . ⊩ . (A -> C))

;; r ⊩ (C -> A) => true
(r . ⊩ . (C -> A))
```

Output for `(r . ⊩ . (C -> A))`:

```
Check: r ⊩ (C -> A)
 Check: r ⊩ C
 Result: r ⊮ C
 Check: s ⊩ (C -> A)
  Check: s ⊩ C
  Result: s ⊮ C
 Result: s ⊩ (C -> A)
 Check: t ⊩ (C -> A)
  Check: t ⊩ C
  Result: t ⊮ C
  Check: u ⊩ (C -> A)
   Check: u ⊩ C
   Result: u ⊩ C
   Check: u ⊩ A
   Result: u ⊩ A
  Result: u ⊩ (C -> A)
 Result: t ⊩ (C -> A)
Result: r ⊩ (C -> A)
#t
```



Update: add printing log, add indent