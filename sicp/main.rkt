#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 current-print
                 flush-output
                 make-parameter
                 error
                 void?)
           (rename racket/base racket:module-begin #%module-begin))

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


(#%provide (rename sicp-random random))
(#%require (only racket random))
(define (sicp-random n)
  (if (and (integer? n) (exact? n))
      (random n)
      (* n (random))))

(#%provide amb)
(define amb-fail '*)

(define (initialize-amb-fail)
  (set! amb-fail
    (lambda ()
      (display "amb tree exhausted"))))

(initialize-amb-fail)

(define call/cc call-with-current-continuation)

(define-syntax amb
  (syntax-rules ()
    ((amb alts ...)
     (let ((prev-amb-fail amb-fail))
       (call/cc
        (lambda (sk)
          (call/cc
           (lambda (fk)
             (set! amb-fail
               (lambda ()
                 (set! amb-fail prev-amb-fail)
                 (fk 'fail)))
             (sk alts))) ...
             (prev-amb-fail)))))))
