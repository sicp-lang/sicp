#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 current-print
                 error
                 flush-output
                 make-parameter
                 random
                 void?)
           (rename racket/base racket:module-begin #%module-begin)
           (rename racket/base random* random))

(#%provide (all-from-except r5rs #%module-begin)
           (rename racket:module-begin #%module-begin))

(#%provide true)
(define true #t)

(#%provide false)
(define false #f)

(#%provide nil)
(define nil '())

(#%provide identity)
(define (identity x) x)

(#%provide inc)
(define (inc x) (+ x 1))

(#%provide dec)
(define (dec x) (- x 1))

(#%provide runtime)
(define (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))

(#%provide cons-stream)
(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(#%provide the-empty-stream)
(define the-empty-stream '())

(#%provide stream-null?)
(define (stream-null? x) (null? x))

(#%provide error)

(#%provide random)
(define (random n)
  (if (and (exact? n) (integer? n))
      (random* n)
      (* n (random*))))
