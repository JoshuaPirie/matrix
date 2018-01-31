#lang racket

(provide (all-defined-out))

(define/contract (matrix? m)
  predicate/c
  (and (list? m)
       (not (= (length m) 0))
       (andmap matrix-row? m)
       (or (= (length m) 1)
           (apply = (map length m)))))

(define/contract (matrix-row? r)
  predicate/c
  (and (list? r)
       (not (= (length r) 0))
       (andmap matrix-val? r)))

(define/contract (matrix-col? c)
  predicate/c
  (matrix-row? c))

(define/contract (matrix-val? v)
  predicate/c
  (number? v))
