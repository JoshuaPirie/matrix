#lang racket

(require "matrix-types.rkt"
         "matrix-operations.rkt")

(provide (all-defined-out))

(define/contract (make-matrix r [c r] [v 0])
  ((exact-positive-integer?) (exact-positive-integer? matrix-val?) . ->* . matrix?)
  (make-list r
             (make-list c v)))

(define/contract (build-matrix r [c r] [f (const 0)])
  ((exact-positive-integer?)
   (exact-positive-integer?
    (natural? natural? . -> . matrix-val?)) . ->* . matrix?)
  (build-list r
              (λ (x)
                (build-list c
                            (curry f x)))))

(define/contract (identity-matrix n)
  (exact-positive-integer? . -> . matrix?)
  (build-matrix n n
                (λ (x y)
                  (if (= x y) 1 0))))

(define/contract (row-matrix l)
  (matrix-row? . -> . matrix?)
  (list l))

(define/contract (col-matrix l)
  (matrix-row? . -> . matrix?)
  (matrix-transpose (list l)))

(define/contract (diagonal-matrix l)
  (matrix-row? . -> . matrix?)
  (build-matrix (length l) (length l)
                (λ (x y)
                  (if (= x y) (list-ref l x) 0))))
