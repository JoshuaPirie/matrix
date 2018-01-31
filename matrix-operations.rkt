#lang racket

(require "matrix-types.rkt"
         "matrix-utils.rkt")

(provide matrix+
         matrix-
         matrix-hadamard
         matrix*
         matrix-conjugate
         matrix-transpose
         matrix-hermitian
         matrix-rev-rows
         matrix-rev-cols
         matrix-rot-90
         matrix-rot-180
         matrix-rot-270
         matrix-map)

(define (matrix-oper-val? v)
  (or (matrix-val? v)
      (matrix? v)))

(define (matrix-oper proc oper . args)
  (case (length args)
    [(0) (oper)]
    [(1) (if (matrix-val? (first args))
             (oper (first args))
             (matrix-map oper (first args)))]
    [else (matrix-pair-oper proc oper
                            (first args)
                            (if (= (length (rest args)) 1)
                                (second args)
                                (apply matrix-oper proc oper (rest args))))]))

(define (matrix-pair-oper proc oper m n)
  (if (matrix-val? m)
      (if (matrix-val? n)
          (oper m n)
          (matrix-scalar-oper oper m n))
      (if (matrix-val? n)
          (matrix-scalar-oper oper n m)
          (proc oper m n))))

(define (matrix-scalar-oper oper scalar m)
  (matrix-map (curry oper scalar) m))

(define (matrix-entrywise-oper oper m n)
  (if (and (= (matrix-num-rows m) (matrix-num-rows n))
           (= (matrix-num-cols m) (matrix-num-cols n)))
      (map (λ (m-row n-row)
             (map oper m-row n-row))
           m n)
      (raise-arguments-error 'matrix-entrywise-oper
                             "matrices are of differing dimensions"
                             "matrix a" (matrix-dim-str m)
                             "matrix b" (matrix-dim-str n))))

(define/contract (matrix+ . args)
  (() #:rest (listof matrix-oper-val?) . ->* . matrix-oper-val?)
  (apply matrix-oper matrix-entrywise-oper + args))

(define/contract (matrix- . args)
  ((matrix-oper-val?) #:rest (listof matrix-oper-val?) . ->* . matrix-oper-val?)
  (apply matrix-oper matrix-entrywise-oper - args))

(define/contract (matrix-hadamard . args)
  (() #:rest (listof matrix-oper-val?) . ->* . matrix-oper-val?)
  (apply matrix-oper matrix-entrywise-oper * args))

(define/contract (matrix* . args)
  (() #:rest (listof matrix-oper-val?) . ->* . matrix-oper-val?)
  (apply matrix-oper (λ (o m n)
                       (if (= (matrix-num-cols m) (matrix-num-rows n))
                           (let ([n (matrix-transpose n)])
                             (map (λ (m-row)
                                    (map (λ (n-col)
                                           (foldl + 0 (map o m-row n-col)))
                                         n))
                                  m))
                           (raise-arguments-error 'matrix-mul
                                                  "matrix a's column dimension does not match matrix b's row dimension"
                                                  "matrix a cols" (matrix-num-cols m)
                                                  "matrix b rows" (matrix-num-rows n))))
         * args))

(define/contract (matrix-conjugate m)
  (matrix? . -> . matrix?)
  (matrix-map (λ (x) (conjugate x)) m))

(define/contract (matrix-transpose m)
  (matrix? . -> . matrix?)
  (apply map list m))

(define/contract (matrix-hermitian m)
  (matrix? . -> . matrix?)
  (matrix-transpose (matrix-conjugate m)))

(define (reverse l)
  (let loop ([l l] [rev '()])
    (if (empty? l)
        rev
        (loop (rest l) (cons (first l) rev)))))

(define/contract (matrix-rev-rows m)
  (matrix? . -> . matrix?)
  (reverse m))

(define/contract (matrix-rev-cols m)
  (matrix? . -> . matrix?)
  (map reverse m))

(define/contract (matrix-rot-90 m)
  (matrix? . -> . matrix?)
  (matrix-transpose (matrix-rev-rows m)))

(define/contract (matrix-rot-180 m)
  (matrix? . -> . matrix?)
  (matrix-rev-rows (matrix-rev-cols m)))

(define/contract (matrix-rot-270 m)
  (matrix? . -> . matrix?)
  (matrix-transpose (matrix-rev-cols m)))

(define/contract (matrix-map f m)
  ((matrix-val? . -> . matrix-val?) matrix? . -> . matrix?)
  (map (λ (m-row) (map f m-row)) m))
