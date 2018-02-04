#lang racket

(require "matrix-types.rkt")

(provide (all-defined-out))

(define/contract (row-matrix? m)
  predicate/c
  (and (matrix? m) (= (matrix-num-rows m) 1)))

(define/contract (col-matrix? m)
  predicate/c
  (and (matrix? m) (= (matrix-num-cols m) 1)))

(define/contract (zero-matrix? m)
  predicate/c
  (and (matrix? m) (andmap (curry andmap zero?) m)))

(define/contract (square-matrix? m)
  predicate/c
  (and (matrix? m) (= (matrix-num-rows m) (matrix-num-cols m))))

(define/contract (matrix-num-rows m)
  (matrix? . -> . exact-positive-integer?)
  (length m))

(define/contract (matrix-num-cols m)
  (matrix? . -> . exact-positive-integer?)
  (length (first m)))

(define/contract (matrix-shape m)
  (matrix? . -> . (values exact-positive-integer? exact-positive-integer?))
  (values (matrix-num-rows m) (matrix-num-cols m)))

(define/contract (matrix-dim-str m)
  (matrix? . -> . string?)
  (let-values (((r c) (matrix-shape m)))
    (string-append (number->string r) "x" (number->string c))))

(define/contract (square-matrix-size m)
  (square-matrix? . -> . exact-positive-integer?)
  (matrix-num-rows m))

(define/contract (matrix-row m n)
  (matrix? natural? . -> . matrix-row?)
  (list-ref m n))

(define/contract (matrix-col m n)
  (matrix? natural? . -> . matrix-col?)
  (build-list (matrix-num-rows m)
              (λ (y) (matrix-val m n y))))

(define/contract (matrix-diagonal m)
  (matrix? . -> . matrix-row?)
  (build-list (matrix-num-rows m)
              (λ (y) (matrix-val m y y))))

(define/contract (matrix-val m x y)
  (matrix? natural? natural? . -> . matrix-val?)
  (list-ref (list-ref m y) x))

(define/contract (matrix-set m x y v)
  (matrix? natural? natural? matrix-val? . -> . matrix?)
  (list-set m y (list-set (matrix-row m y) x v)))
